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
	Plugin 'rakr/vim-one'
	Plugin 'ntk148v/vim-horizon'

	" File management
	" Plugin 'scrooloose/nerdtree'
	" Plugin 'tpope/vim-vinegar'

	" Git
	Plugin 'jreybert/vimagit'
	Plugin 'tpope/vim-fugitive'
	
	" File support
	Plugin 'cespare/vim-toml'
	" Plugin 'vim-scripts/TagHighlight'
	Plugin 'lervag/vimtex'
	Plugin 'LnL7/vim-nix'
	" Plugin 'rust-lang/rust.vim'
	" Plugin 'racer-rust/vim-racer'

	Plugin 'junegunn/vim-easy-align'
	call vundle#end()            " required
	filetype plugin indent on    " required



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
	" let g:rustfmt_autosave = 1

	autocmd FileType c let b:vcm_tab_complete = 'omni'
	autocmd FileType go let b:vcm_tab_complete = 'omni'
	autocmd FileType vim let b:vcm_tab_complete = 'vim'

	set completeopt=menuone

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

	let g:netrw_liststyle=2
	let g:netrw_banner = 0

" autocmd BufEnter * call ncm2#enable_for_buffer()
" let g:ncm2#auto_popup = 0

" Keymapping
	map <F4> :TagbarToggle<CR>

	nnoremap <C-b> :b 

	nnoremap <silent> <Plug>QuickNext :cnext<CR> :call repeat#set("\<Plug>QuickNext")<CR>
	nnoremap <silent> <Plug>QuickPrev :cprev<CR> :call repeat#set("\<Plug>QuickPrev")<CR>
	nnoremap ,, :copen<CR><c-w><c-p>

	cnoreabbrev ln lnext
	cnoreabbrev lp lprev
	cnoreabbrev cn cnext
	cnoreabbrev cp cprev

	nmap <C-j> <Plug>QuickNext
	nmap <C-k> <Plug>QuickPrev
	nmap - :Exp<CR>

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

		" Start interactive EasyAlign in visual mode (e.g. vipga)
		xmap ga <Plug>(EasyAlign)

		" Start interactive EasyAlign for a motion/text object (e.g. gaip)
		nmap ga <Plug>(EasyAlign)

" Enforcing filetypes
	autocmd BufRead,BufNewFile *.ino set filetype=c
	autocmd BufRead,BufNewFile *.asc set filetype=asciidoc
	autocmd FileType python setlocal completeopt-=preview

" Highlightning
	if (empty($TMUX))
		if (has("nvim"))
			"For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
			let $NVIM_TUI_ENABLE_TRUE_COLOR=1
		endif
		"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
		"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
		" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
		if (has("termguicolors"))
			set termguicolors
		endif
	endif
	set background=dark
	let g:one_allow_italics = 1
	colorscheme one


" Spell check
	set spelllang=en
	autocmd FileType tex set spell

" Latex support
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

" Terrible oneliner for debugging color highlightning
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
