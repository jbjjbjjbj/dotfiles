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
    Plugin 'prabirshrestha/async.vim'
    Plugin 'prabirshrestha/vim-lsp'
    Plugin 'lifepillar/vim-mucomplete'
    Plugin 'Shougo/echodoc.vim'

    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-repeat'
    Plugin 'tpope/vim-unimpaired'
    Plugin 'tpope/vim-commentary'

    Plugin 'godlygeek/tabular'

    " Editor config
    " Plugin 'editorconfig/editorconfig-vim'

    " Color theme
    " Plugin 'agude/vim-eldar'
    " Plugin 'ntk148v/vim-horizon'
    " Plugin 'rakr/vim-colors-rakr'
    " Plugin 'rakr/vim-one'
    Plugin 'morhetz/gruvbox'
    " Plugin 'ntk148v/vim-horizon'

    " Git
    Plugin 'tpope/vim-fugitive'
    
    " File support
    " Plugin 'cespare/vim-toml'
    "Plugin 'lervag/vimtex'
    
    " Plugin 'vimwiki/vimwiki'
    

    " Snippits
    Plugin 'SirVer/ultisnips'
    Plugin 'honza/vim-snippets'

    call vundle#end()            " required
    filetype plugin indent on    " required

" Completion
    set completeopt+=menuone
    set completeopt+=noselect

    let g:UltiSnipsExpandTrigger = '<C-s>'
    let g:mucomplete#chains = {}
    let g:mucomplete#chains.default = [ 'omni', 'ulti', 'path', 'keyn' ]

    if has('nvim')
        let g:echodoc#enable_at_startup = 1
        let g:echodoc#type = 'virtual'
    endif

    " Lsp options
    let g:lsp_signature_help_enabled = 0
    let g:lsp_insert_text_enabled = 0
    let g:lsp_virtual_text_enabled = 0
    let g:lsp_diagnostics_echo_cursor = 1
    let g:lsp_highlights_enabled = 0
    let g:lsp_textprop_enabled = 0
    let g:lsp_signs_enabled = 1

    " Setup lsp servers
    if executable('pyls')
        au User lsp_setup call lsp#register_server({
        			\ 'name': 'python',
        			\ 'cmd': {server_info->['pyls']},
        			\ 'whitelist': ['python'],
        			\ })
    endif
    if executable('clangd')
        au User lsp_setup call lsp#register_server({
        			\ 'name': 'clangd',
        			\ 'cmd': {server_info->['clangd']},
        			\ 'whitelist': ['c', 'cpp'],
        			\ })
    endif
    if executable('gopls')
        au User lsp_setup call lsp#register_server({
        			\ 'name': 'golang',
        			\ 'cmd': {server_info->['gopls']},
        			\ 'whitelist': ['go'],
        			\ })
    endif
    if executable('rls')
        au User lsp_setup call lsp#register_server({
            \ 'name': 'rust',
            \ 'cmd': {server_info->['rls']},
            \ 'whitelist': ['rust', 'rs'],
            \ })
    endif
    if executable('solargraph')
        au User lsp_setup call lsp#register_server({
            \ 'name': 'ruby',
            \ 'cmd': {server_info->['solargraph', 'stdio']},
            \ 'whitelist': ['ruby', 'rb'],
            \ })
    endif

    " Enable csp if available, stolen from lsp github
    function! s:on_lsp_buffer_enabled() abort
        echo "Enabling lsp"
        set omnifunc=lsp#complete
        set signcolumn=yes
        nmap <buffer> gd <plug>(lsp-definition)
        nmap <buffer> <f2> <plug>(lsp-rename)
        " refer to doc to add more commands
    endfunction

    augroup lsp_install
        au!
        " call s:on_lsp_buffer_enabled only for languages that has the server registered.
        autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
    augroup END

" General vim settings
    syntax enable
    set number 
    set relativenumber

    " More sensible splitting
    set splitbelow splitright

    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set expandtab
    set colorcolumn=80

    " Show 5 lines above and below cursor
    set scrolloff=3
    set list

    " Configure statusline
    set laststatus=1

    if has('nvim')
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
    " au CursorHoldI * stopinsert

" Vimwiki stuff
    let g:vimwiki_list = [{'path': '~/Documents/vimwiki', 'path_html': '~/Documents/vimwiki/export'}]

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

" File management
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
    autocmd FileType tex,markdown,rst set spell
