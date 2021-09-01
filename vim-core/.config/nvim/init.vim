set nocompatible
filetype plugin on
set shellslash

runtime extra.vim
filetype plugin indent on    " required

" General vim settings{{{
    syntax enable
    set number 
    set relativenumber

    set cul

    " More sensible splitting
    set splitbelow splitright

    set backspace=start,eol

    " Do not create the tilde files
    set nobackup

    set tabstop=4
    set shiftwidth=4
    set expandtab
    set colorcolumn=80

    set softtabstop=4

    set autoindent
    set smartindent

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

    " Exit insert mode on inactivity
    " au CursorHoldI * stopinsert}}}

" Cursor{{{
    augroup cursorgroup
        autocmd InsertEnter * set nocul
        autocmd InsertLeave * set cul
    augroup end"}}}

" Language specific{{{
    autocmd FileType python set et
    autocmd FileType go setlocal noet
    autocmd FileType nix setlocal shiftwidth=4 softtabstop=4
    autocmd FileType vim setlocal foldmethod=marker"}}}

" File management{{{
    " Fuzzy like menu
    set path+=**
    set wildmenu

    let g:netrw_liststyle=2"}}}

" Keymapping{{{
    " Tab for cycling the completion meny, in insert mode
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

    " When moving more lines make it a jump. If couns i 2 it will run m'2j,
    " thus storing it on the jumplist and then jumping
    nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '' ) . 'j'
    nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '' ) . 'k'

    " Leader stuff
        let mapleader="\<Space>"

" Enforcing filetypes{{{
    autocmd BufRead,BufNewFile *.ino set filetype=c
    autocmd BufRead,BufNewFile *.asc set filetype=asciidoc
    autocmd BufRead,BufNewFile *.nix set filetype=nix

    autocmd BufRead,BufNewFile *.tsx set filetype=typescript"}}}

" Highlightning And colors{{{
    set termguicolors

" Spell check
    set spelllang=en,da
    " autocmd FileType mail,tex,markdown,rst set spell}}}

