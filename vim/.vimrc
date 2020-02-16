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
	" Plugin 'rakr/vim-colors-rakr'
	" Plugin 'rakr/vim-one'
	Plugin 'morhetz/gruvbox'
	" Plugin 'ntk148v/vim-horizon'

	" File management
	" Plugin 'scrooloose/nerdtree'
	" Plugin 'tpope/vim-vinegar'

	" Git
	" Plugin 'jreybert/vimagit'
	" Plugin 'tpope/vim-fugitive'
	
	" File support
	" Plugin 'cespare/vim-toml'
	" Plugin 'vim-scripts/TagHighlight'
	Plugin 'lervag/vimtex'
	" Plugin 'LnL7/vim-nix'
	" Plugin 'rust-lang/rust.vim'
	" Plugin 'racer-rust/vim-racer'
	

	" Snippets are separated from the engine. Add this if you want them:
	" Plugin 'honza/vim-snippets'

	call vundle#end()            " required
	filetype plugin indent on    " required

" Completion
	let g:go_def_mode='gopls'
	let g:go_info_mode='gopls'
	" let g:rustfmt_autosave = 1

	" autocmd FileType c let b:vcm_tab_complete = 'omni'
	autocmd FileType c let b:vcm_tab_complete = 'omni'
	autocmd FileType go let b:vcm_tab_complete = 'omni'
	autocmd FileType vim let b:vcm_tab_complete = 'vim'

	set completeopt=menuone

" General vim settings
	syntax enable
	set number 
	set relativenumber

	set tabstop=4
	set shiftwidth=4
	set softtabstop=4
	set noexpandtab

	" Remove statusline
	set laststatus=0

	" Check if file has changed
	au FocusGained,BufEnter * :checktime

	if has('nvim')
		set inccommand=split
	endif

	set hidden

" Latex stuff
	let g:tex_flavor='latex'

	let g:vimtex_quickfix_blgparser = {'disable': 1}
	let g:vimtex_quickfix_open_on_warning = 0
	let g:vimtex_compiler_latexmk = {
        \ 'backend' : 'nvim',
        \ 'background' : 0,
        \ 'build_dir' : '',
        \ 'callback' : 1,
        \ 'continuous' : 0,
        \ 'executable' : 'latexmk',
        \ 'hooks' : [],
        \ 'options' : [
        \   '-file-line-error',
        \   '-synctex=1',
        \ ],
        \}

" File finding
	" Fuzzy like menu
	set path+=**
	set wildmenu

	let g:netrw_liststyle=2
	let g:netrw_banner = 0

" Keymapping
	" When moving more lines make it a jump. If couns i 2 it will run m'2j,
	" thus storing it on the jumplist and then jumping
	nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '' ) . 'j'
	nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '' ) . 'k'

	" Leader stuff
		let mapleader=" "

		map <leader>mm :make V=1<CR>
		map <leader>mf :make flash V=1<CR>

" Enforcing filetypes
	autocmd BufRead,BufNewFile *.ino set filetype=c
	autocmd BufRead,BufNewFile *.asc set filetype=asciidoc

" Highlightning And colors
	set termguicolors
	" let g:one_allow_italics = 1
	let g:gruvbox_italic = 1
	colorscheme gruvbox


" Spell check
	set spelllang=en
	autocmd FileType tex set spell
