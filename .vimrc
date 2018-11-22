
set nocompatible              " be iMproved, required
filetype plugin on

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-fugitive'

Plugin 'vim-latex/vim-latex'

Plugin 'racer-rust/vim-racer'

Plugin 'zchee/deoplete-jedi'

if has("nvim")
	Plugin 'Shougo/deoplete.nvim'
else
	Plugin 'Shougo/deoplete.nvim'
	Plugin 'roxma/nvim-yarp'
	Plugin 'roxma/vim-hug-neovim-rpc'
endif

Plugin 'agude/vim-eldar'
Plugin 'scrooloose/nerdtree'

" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

call vundle#end()            " required
filetype plugin indent on    " required


colorscheme eldar

syntax enable
set number relativenumber

set tabstop=4
set shiftwidth=4
set softtabstop=4
set noexpandtab

let g:deoplete#enable_at_startup = 1

tnoremap <C-e> <C-\><C-n>

" Map C-s
noremap <silent> <C-S>     :update<CR>

map <C-n> :NERDTreeToggle<CR>

" Required for operations modifying multiple buffers like rename.
set hidden


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" File finding
set path+=**
set wildmenu

let g:netrw_liststyle=3


set inccommand=split
autocmd FileType python setlocal completeopt-=preview
