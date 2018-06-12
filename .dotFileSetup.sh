sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# clone
git clone https://github.com/powerline/fonts.git --depth=1
# install
cd fonts
./install.sh
# clean-up a bit
cd ..
rm -rf fonts

mkdir /home/julian/.oh-my-zsh/custom/themes 

wget https://raw.githubusercontent.com/caiogondim/bullet-train-oh-my-zsh-theme/master/bullet-train.zsh-theme -O /home/julian/.oh-my-zsh/custom/themes/bullet-train.zsh-theme

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
echo ".cfg" >> .gitignore
git clone --bare 'https://github.com/jbjjbjjbj/newDotFiles' $HOME/.cfg
config config --local status.showUntrackedFiles no
config checkout -f

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
