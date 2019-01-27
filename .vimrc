
set nocompatible              " be iMproved, required
filetype plugin on

" Plugins
	" set the runtime path to include Vundle and initialize
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	" alternatively, pass a path where Vundle should install plugins
	"call vundle#begin('~/some/path/here')

	" let Vundle manage Vundle, required
	Plugin 'VundleVim/Vundle.vim'

	" Completion
	Plugin 'ervandew/supertab'

	Plugin 'tpope/vim-surround'
	Plugin 'tpope/vim-repeat'

	" ctags stuff
	Plugin 'ludovicchabant/vim-gutentags'

	" Language support
	Plugin 'racer-rust/vim-racer'
	Plugin 'davidhalter/jedi-vim'
	Plugin 'junegunn/goyo.vim'

	" Color theme
	Plugin 'agude/vim-eldar'

	" File management
	Plugin 'scrooloose/nerdtree'

	" Snippits
	Plugin 'SirVer/ultisnips'
	Plugin 'honza/vim-snippets'

	" Git
	Plugin 'jreybert/vimagit'

	" Tagbar use <F4>
	Plugin 'majutsushi/tagbar'


	call vundle#end()            " required
	filetype plugin indent on    " required


colorscheme eldar

syntax enable
set number relativenumber

set tabstop=4
set shiftwidth=4
set softtabstop=4
set noexpandtab


" Required for operations modifying multiple buffers like rename.
set hidden

" Snippits
	" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
	let g:UltiSnipsExpandTrigger="<c-b>"
	let g:UltiSnipsJumpForwardTrigger="<c-b>"
	let g:UltiSnipsJumpBackwardTrigger="<c-z>"

	" If you want :UltiSnipsEdit to split your window.
	let g:UltiSnipsEditSplit="vertical"

	" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
	" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

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
