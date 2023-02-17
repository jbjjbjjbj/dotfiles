use clap::Parser;
use serde::Deserialize;
use regex::RegexSet;
use rand::thread_rng;
use rand::seq::SliceRandom;

use std::fs::File;
use std::error;
use std::path::{PathBuf, Path};
use std::process::Command;
use std::fmt;
use std::io;
use std::io::Write;

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    #[arg(short, long, help="Where to load configuration from")]
    config: String,

    #[arg(short, long, help="Reapply last applied wallpaper")]
    reapply: bool,

    #[arg(short, long, help="Choose a specific wallpaper")]
    specific: bool,
}

#[derive(Debug, Deserialize, Clone, Copy)]
enum Rule {
    Tiled,
    Scale,
    Border,
    Center,
}

#[derive(Debug, Deserialize)]
struct Config {
    #[serde(default)]
    rules: Vec<(String, Rule)>,
    #[serde(default = "default_background")]
    background: String,

    #[serde(skip, default = "default_folder")]
    folder: String,
}

#[derive(Debug)]
enum Error {
    FehFailure(i32),
    NoRule(String),
    NoState,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::FehFailure(code) => write!(f, "Feh returned non error: {}", code),
            Error::NoRule(fname) => write!(f, "No rule for file with name {}", fname),
            Error::NoState => write!(f, "There is no previous wallpaper to reapply"),
        }
    }
}
impl error::Error for Error {}

// Default used if we could not get the parent folder of the config file
fn default_folder() -> String { String::from("/") }
fn default_background() -> String { String::from("black") }

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Args::parse();

    // Load configuration file
    let config = Config::parse(&Path::new(&args.config))?;

    if args.reapply {
        let img = state::load_state()?.ok_or(Box::new(Error::NoState))?;
        config.reapply_image_file(&img)?;

        return Ok(());
    } else if args.specific {
        let files = config.get_images()?;
        let choices: Vec<&str> = files.iter().map(|i|
                                                    i.0.file_name().unwrap().to_str().unwrap()
                                                    ).collect();
        if let Some((index, _)) = choice(&choices)? {
            config.reapply_image_file(&files[index].0)?;
        } else {
            println!("No images chosen, doing nothing");
        }
        return Ok(());
    }

    if let Some((img, rule)) = config.select_image_file()? {
        config.apply_image_file(&img, rule)?;
    }

    Ok(())

}

fn choice<'a, T: fmt::Display>(choices: &'a [T]) -> Result<Option<(usize, &'a T)>, Box<dyn error::Error>>{

    for (i, choice) in choices.iter().enumerate() {
        println!("{}: {}", i, choice);
    }

    print!("Which one?: ");
    io::stdout().flush()?;
    let mut choice = String::new();
    io::stdin().read_line(&mut choice)?;
    choice.pop();

    if choice == "" || choice == "q" {
        return Ok(None)
    }

    let choice = choice.parse::<usize>()?;
    if choice < choices.len() {
        Ok(Some((choice, &choices[choice])))
    } else {
        Ok(None)
    }
}

impl Config {
    fn parse(file: &Path) -> Result<Self, Box<dyn error::Error>> {
        let f = File::open(file)?;
        let mut config: Config = serde_yaml::from_reader(f)?;

        if let Some(folder) = file.parent() {
            config.folder = folder.to_string_lossy().into();
        }


        Ok(config)
    }

    fn build_regexset(&self) -> Result<RegexSet, Box<dyn error::Error>> {
        Ok(RegexSet::new(self.rules.iter().map(|(r,_)| r))?)
    }

    fn reapply_image_file(&self, img: &Path) -> Result<(), Box<dyn error::Error>> {
        let rs = self.build_regexset()?;

        // TODO handle the invalid unicode here
        let file_name = img.file_name().ok_or(
            Box::new(Error::NoState))?.to_str().unwrap();
        // Extract the `index` of the first matching rule
        if let Some(index) = rs.matches(file_name).iter().next() {
            self.apply_image_file(img, self.rules[index].1)
        } else {
            // Ohh no, no rules matched
            Err(Box::new(Error::NoRule(file_name.into())))
        }
    }

    fn get_images(&self) -> Result<Vec<(PathBuf, Rule)>, Box<dyn error::Error>> {
        let folder = Path::new(&self.folder);
        let rs = self.build_regexset()?;
        let mut imgs: Vec<(PathBuf, Rule)> = Vec::new();

        for entry in folder.read_dir()? {
            let entry = entry?;
            let file_name = String::from(entry.file_name().to_string_lossy());
            if let Some(index) = rs.matches(&file_name).iter().next() {
                imgs.push((entry.path(), self.rules[index].1));
            }
            // deja vu
        }

        Ok(imgs)
    }

    fn select_image_file(&self) -> Result<Option<(PathBuf, Rule)>, Box<dyn error::Error>> {
        let imgs = self.get_images()?;

        Ok(imgs.choose(&mut thread_rng()).cloned())
    }

    fn apply_image_file(&self, img: &Path, rule: Rule) -> Result<(), Box<dyn error::Error>> {
        println!("Setting wallpaper {:?}, with {:?} mode", img, rule);
        let mode = match rule {
            Rule::Tiled => "--bg-tile",
            Rule::Scale => "--bg-fill",
            Rule::Border => "--bg-max",
            Rule::Center => "--bg-center",
        };

        let status = Command::new("feh").arg(mode).
            arg("--image-bg").arg(&self.background).arg(img).status()?;
        if status.success() {
            state::save_state(img)
        } else {
            Err(Box::new(Error::FehFailure(status.code().unwrap())))
        }
    }
}

mod state {
    use std::error;
    use std::path::{Path,PathBuf};
    use std::fs;

    // Hehe, not a very cool default but whatever
    static FALLBACK_DATADIR: &str = ".";

    fn state_file() -> PathBuf {
        dirs::data_dir().unwrap_or(FALLBACK_DATADIR.into()).join("wallstate")
    }

    pub fn load_state() -> Result<Option<PathBuf>, Box<dyn error::Error>> {
        let stf = state_file();

        if stf.exists() {
            let raw = fs::read(stf)?;
            let content = String::from_utf8_lossy(&raw);
            // Extract only the first line
            Ok(content.lines().next().map(|x| x.into()))
        } else {
            Ok(None)
        }
    }

    pub fn save_state(img: &Path) -> Result<(), Box<dyn error::Error>> {
        Ok(fs::write(state_file(), img.to_str().unwrap())?)
    }
}
