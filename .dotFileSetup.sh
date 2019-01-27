sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install
cd fonts
./install.sh
# clean-up a bit
cd ..
rm -rf fonts

mkdir /home/julian/.oh-my-zsh/custom/themes 

wget https://raw.githubusercontent.com/caiogondim/bullet-train-oh-my-zsh-theme/master/bullet-train.zsh-theme -O /home/julian/.oh-my-zsh/custom/themes/bullet-train.zsh-theme

cd ~/.oh-my-zsh/custom/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
echo ".cfg" >> .gitignore
git clone --bare 'https://github.com/jbjjbjjbj/newDotFiles' $HOME/.cfg
config config --local status.showUntrackedFiles no
config checkout -f

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

mkdir Software
cd Software

git clone https://github.com/jbjjbjjbj/Custom-dwm6.1.git
cd Custom-dwm6.1
sudo make install
cd ..

git clone https://github.com/jbjjbjjbj/Custom-st.git
cd Custom-st
sudo make install
cd
