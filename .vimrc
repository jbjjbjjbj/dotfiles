
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'mileszs/ack.vim'

Plugin 'autozimu/LanguageClient-neovim'

Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'

Plugin 'vim-latex/vim-latex'
Plugin 'vim-scripts/ReplaceWithRegister'

Plugin 'zchee/deoplete-jedi'
Plugin 'sirtaj/vim-openscad'
Plugin 'tmhedberg/matchit'
Plugin 'kien/ctrlp.vim'
Plugin 'Shougo/deoplete.nvim'

Plugin 'agude/vim-eldar'

Plugin 'scrooloose/nerdtree'

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


inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


tnoremap <C-e> <C-\><C-n>

" Map C-s
noremap <silent> <C-S>     :update<CR>

" Make ToBegin and ToEnd
function! InputChar()
    let c = getchar()
    return type(c) == type(0) ? nr2char(c) : c
endfunction

function! ToEnd()
    let s =  InputChar()
    if s =~ "\<esc>" || s =~ "\<c-c>"  
        return
    endif
    execute "normal! vi". s. "\<Esc>"
endfunction

function! ToBegin()
    call ToEnd()
    execute "'<"
endfunction

nnoremap <silent> ge :call ToEnd()<cr>
nnoremap <silent> gb :call ToBegin()<cr>

map <C-n> :NERDTreeToggle<CR>

" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls'],
    \ }

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <F5> :call LanguageClient_contextMenu()<CR>

