
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

"nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
"nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
"nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
"nnoremap <F5> :call LanguageClient_contextMenu()<CR>


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


" Run  binding
nnoremap <F5> :!terminator -e '"%:p" ;read -n 1'<CR>
set completeopt-=preview


" File finding
set path+=**
set wildmenu

let g:netrw_liststyle=3


" Snippits

nnoremap ,html :-1read $HOME/.vim/snippets/skeleton.html<CR>
