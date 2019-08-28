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
	"Plugin 'prabirshrestha/async.vim'
	"Plugin 'prabirshrestha/vim-lsp'
	Plugin 'fatih/vim-go'
	Plugin 'ajh17/VimCompletesMe'
	Plugin 'ludovicchabant/vim-gutentags'

	Plugin 'tpope/vim-surround'
	Plugin 'tpope/vim-repeat'

	" Editor config
	Plugin 'editorconfig/editorconfig-vim'

	" Color theme
	" Plugin 'agude/vim-eldar'
	" Plugin 'ntk148v/vim-horizon'
	Plugin 'rakr/vim-colors-rakr'

	" File management
	" Plugin 'scrooloose/nerdtree'
	Plugin 'tpope/vim-vinegar'

	" Git
	" Plugin 'jreybert/vimagit'
	
	" File support
	Plugin 'cespare/vim-toml'

	" Tagbar use <F4>
	Plugin 'majutsushi/tagbar'

	call vundle#end()            " required
	filetype plugin indent on    " required


colorscheme rakr

" Completion
	" let g:LanguageClient_serverCommands = {
	" 	\ 'rust': ['/usr/bin/rustup', 'run', 'stable', 'rls' ],
	" 	\ 'c': ['/usr/bin/cquery', '--log-file=/tmp/cq.log', '--init={"cacheDirectory":"/tmp/cquery/"}' ]
	" 	\ }
	" 
	" let g:LanguageClient_useVirtualText = 0
	" let g:LanguageClient_diagnosticsEnable = 0
	" let g:deoplete#enable_at_startup = 1
	let g:go_def_mode='gopls'
	let g:go_info_mode='gopls'

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
	map <F4> :TagbarToggle<CR>

	nnoremap <C-b> :b 

	nnoremap <silent> <Plug>QuickNext :cnext<CR> :call repeat#set("\<Plug>QuickNext")<CR>
	nnoremap <silent> <Plug>QuickPrev :cprev<CR> :call repeat#set("\<Plug>QuickPrev")<CR>
	nnoremap ,, :copen<CR><c-w><c-p>

	nmap ,n <Plug>QuickNext
	nmap ,N <Plug>QuickPrev

	" When moving more lines make it a jump. If couns i 2 it will run m'2j,
	" thus storing it on the jumplist and then jumping
	nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '' ) . 'j'
	nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '' ) . 'k'

	" Leader stuff
		let mapleader=" "

		map <leader>mm :make V=1<CR>
		map <leader>mf :make flash V=1<CR>

		" Example on filetype specific
		" autocmd FileType tex map <leader>o :w !detex \| wc -w<CR>
		autocmd FileType asciidoc nnoremap <leader>c :!asciidoctor %<CR>

" Enforcing filetypes
	autocmd BufRead,BufNewFile *.ino set filetype=c
	autocmd BufRead,BufNewFile *.asc set filetype=asciidoc
	autocmd FileType python setlocal completeopt-=preview

