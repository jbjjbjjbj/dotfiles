set nocompatible
filetype plugin indent on
set shellslash

let mapleader=" "
runtime extra.vim
filetype plugin indent on    " required

" General vim settings{{{
    syntax enable
    set number 
    set relativenumber

    set cul

    " More sensible splitting
    set splitbelow splitright

    set backspace=start,eol,indent

    " Do not create the tilde files
    set nobackup

    set tabstop=2
    " when zero we use tabstop
    set shiftwidth=0
    set expandtab
    set colorcolumn=80

    " Use the value to shiftwidth here
    set softtabstop=-1

    set autoindent

    " Show 5 lines above and below cursor
    set scrolloff=3
    set list
    set listchars=tab:>\ ,trail:-,nbsp:+

    " Configure statusline
    set laststatus=1

    set incsearch

    if exists('&inccommand')
        " Preview substitution
        set inccommand=split
    endif

    set background=dark

    set hidden

    " Case insensitive search if all letters are small
    set smartcase
    set ignorecase

    " Persistent undo
    set undofile

    set autoread

    " set cino=J1,j1,L0,:0,=0,l1,h0

    " Exit insert mode on inactivity
    " au CursorHoldI * stopinsert}}}

" Cursor{{{
    augroup cursorgroup
        autocmd InsertEnter * set nocul
        autocmd InsertLeave * set cul
    augroup end"}}}

" Language specific{{{
  autocmd FileType python
    \ set et |
    \ set tabstop=2 |
    \ set shiftwidth=2 |
  autocmd FileType go setlocal noet
  autocmd FileType vim setlocal foldmethod=marker
  autocmd FileType java setlocal tabstop=4
  autocmd BufNewFile,BufRead *.tex
    \ set nocursorline |
    \ set nornu |
    \ set number |
    \ let g:loaded_matchparen=1 |

"}}}

" File management{{{
    " Fuzzy like menu
    set path=.,,src/**,Dtos/**,Controllers/**,Extensions/**,Helpers/**,Migrations/**,Integrations/**,Model/**,Services/**,Properties/**,apps/**,libs/**
    set wildmenu
    set wildignore+=**/node_modules/**

    let g:netrw_liststyle=2"}}}

" Keymapping{{{
    " Tab for cycling the completion meny, in insert mode
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

    " When moving more lines make it a jump. If couns i 2 it will run m'2j,
    " thus storing it on the jumplist and then jumping
    nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '' ) . 'j'
    nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '' ) . 'k'

    " Leader stuff
    nnoremap <leader>t <cmd>echo "Yeah det virker"<cr>

" Enforcing filetypes{{{
    autocmd BufRead,BufNewFile *.ino set filetype=c
    autocmd BufRead,BufNewFile *.asc set filetype=asciidoc
    autocmd BufRead,BufNewFile *.nix set filetype=nix

    autocmd BufRead,BufNewFile *.tsx set filetype=typescriptreact
    "}}}

" Highlightning And colors{{{
    set termguicolors
    colorscheme PaperColor

" Spell check
    set spelllang=en,da
    " autocmd FileType mail,tex,markdown,rst set spell}}}

