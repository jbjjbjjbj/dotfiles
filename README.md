# newDotFiles

This is where i keep my dotfiles and scripts.

## Dotfile management

I use [this](https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/) method of managing dotfiles

If the link above is broken, here is a short summary.

The method comes from this [thread](https://news.ycombinator.com/item?id=11070797). The idea is to transform your home folder into a git repository, and then add the files you edited.

### First time setup

Make a new bare github repository

Initialize git

`git init --bare $HOME/.cfg`

Add the easy to use alias

`alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'`

Set git option

`config config --local status.showUntrackedFiles no`

This is important, because othervise git vil print every file in your home directory.

Remember to add `alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'` to your shellrc

Now commands like `config add`, `config commit`, `config push` and `config pull` can be used to manage and sync you config files to github.

### Add to new computer

Configure the alias

`alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'`

Make gitignore
`echo ".cfg" >> .gitignore`

Clone repository
`git clone --bare <git-repo-url> $HOME/.cfg`

Set git option

`config config --local status.showUntrackedFiles no`

Checkout
`config checkout`

Backup or delete files that conflict this command and then run `config checkout` again

And turn off the tracking
`config config --local status.showUntrackedFiles no`
