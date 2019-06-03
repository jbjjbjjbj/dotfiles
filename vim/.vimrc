set nocompatible              " be iMproved, required
filetype plugin on
set shellslash


" Plugins
	" set the runtime path to include Vundle and initialize
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	" alternatively, pass a path where Vundle should install plugins
	"call vundle#begin('~/some/path/here')

	" let Vundle manage Vundle, required
	Plugin 'VundleVim/Vundle.vim'

	" Completion
	Plugin 'autozimu/LanguageClient-neovim'
	if has('nvim')
	  Plugin 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
	else
	  Plugin 'Shougo/deoplete.nvim'
	  Plugin 'roxma/nvim-yarp'
	  Plugin 'roxma/vim-hug-neovim-rpc'
	endif

	Plugin 'tpope/vim-surround'
	Plugin 'tpope/vim-repeat'

	" Editor config
	Plugin 'editorconfig/editorconfig-vim'

	" Color theme
	" Plugin 'agude/vim-eldar'
	Plugin 'ntk148v/vim-horizon'
	" Plugin 'fatih/molokai'

	" File management
	" Plugin 'scrooloose/nerdtree'
	Plugin 'tpope/vim-vinegar'

	" Git
	" Plugin 'jreybert/vimagit'

	" Tagbar use <F4>
	Plugin 'majutsushi/tagbar'

	call vundle#end()            " required
	filetype plugin indent on    " required


colorscheme horizon

" Completion
let g:LanguageClient_serverCommands = {
	\ 'rust': ['/usr/bin/rustup', 'run', 'stable', 'rls' ],
	\ 'c': ['/usr/bin/cquery', '--log-file=/tmp/cq.log', '--init={"cacheDirectory":"/tmp/cquery/"}' ]
	\ }

let g:LanguageClient_useVirtualText = 0
let g:LanguageClient_diagnosticsEnable = 0
let g:deoplete#enable_at_startup = 1

syntax enable
set number relativenumber

set tabstop=4
set shiftwidth=4
set softtabstop=4
set noexpandtab

let g:tex_flavor='latex'

if has('nvim')
	set inccommand=split
endif

" Required for operations modifying multiple buffers like rename.
set hidden

" Snippits
	" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
	" let g:UltiSnipsExpandTrigger="<c-b>"
	" let g:UltiSnipsJumpForwardTrigger="<c-b>"
	" let g:UltiSnipsJumpBackwardTrigger="<c-z>"

	" If you want :UltiSnipsEdit to split your window.
	" let g:UltiSnipsEditSplit="vertical"


" File finding
	set path+=**
	set wildmenu

	let g:netrw_liststyle=3

" autocmd BufEnter * call ncm2#enable_for_buffer()
" let g:ncm2#auto_popup = 0
set completeopt=menuone,preview

" Keymapping
	map <C-n> :NERDTreeToggle<CR>
	map <F4> :TagbarToggle<CR>

	inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
	inoremap <expr><C-j> pumvisible() ? "\<C-n>" : "\<C-x>\<C-o>"
	inoremap <expr><C-k> pumvisible() ? "\<C-p>" : "\<C-x>\<C-o>"

	nnoremap <F5> :call LanguageClient_contextMenu()<CR>
	nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
	nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
	nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

	nnoremap <C-b> :b 

	" When moving more lines make it a jump. If couns i 2 it will run m'2j,
	" thus storing it on the jumplist and then jumping
	nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '' ) . 'j'
	nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '' ) . 'k'

	" Leader stuff
		let mapleader=" "

		map <leader>z :Goyo<CR>
		map <leader>mm :make V=1<CR>
		map <leader>mf :!make flash V=1<CR>

		" Example on filetype specific
		" autocmd FileType tex map <leader>o :w !detex \| wc -w<CR>
		autocmd FileType asciidoc nnoremap <leader>c :!asciidoctor %<CR>

" Enforcing filetypes
	autocmd BufRead,BufNewFile *.ino set filetype=c
	autocmd BufRead,BufNewFile *.asc set filetype=asciidoc
	autocmd FileType python setlocal completeopt-=preview

" Path settings
	let g:ycm_rust_src_path = '~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
