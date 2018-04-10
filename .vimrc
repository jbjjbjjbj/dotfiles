
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-speeddating'

Plugin 'vim-latex/vim-latex'

Plugin 'vim-erlang/vim-erlang-runtime'

Plugin 'zchee/deoplete-jedi'
Plugin 'leafgarland/typescript-vim'
Plugin 'sirtaj/vim-openscad'
Plugin 'tmhedberg/matchit'
Plugin 'nanotech/jellybeans.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'Shougo/deoplete.nvim'

Plugin 'agude/vim-eldar'

call vundle#end()            " required
filetype plugin indent on    " required


colorscheme eldar

syntax enable
set number relativenumber

set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab


let g:deoplete#enable_at_startup = 1


inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


tnoremap <C-e> <C-\><C-n>
