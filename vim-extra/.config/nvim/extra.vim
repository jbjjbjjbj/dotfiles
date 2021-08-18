" Plugins{{{
    " set the runtime path to include Vundle and initialize
    set rtp+=~/.config/nvim/bundle/Vundle.vim
    call vundle#begin("~/.config/nvim/bundle")
    " alternatively, pass a path where Vundle should install plugins
    "call vundle#begin('~/some/path/here')

    " let Vundle manage Vundle, required
    Plugin 'VundleVim/Vundle.vim'

    " Completion
    "Plugin 'Shougo/deoplete.nvim'
    Plugin 'ajh17/VimCompletesMe'
    " Plugin 'prabirshrestha/vim-lsp'
    Plugin 'autozimu/LanguageClient-neovim'
    Plugin 'Shougo/echodoc.vim'

    " Installed through vim-core
    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-repeat'
    Plugin 'tpope/vim-unimpaired'
    Plugin 'tpope/vim-commentary'

    "Plugin 'godlygeek/tabular'

    " Editor config
    Plugin 'sgur/vim-editorconfig'

    " Color theme
    Plugin 'agude/vim-eldar'
    " Plugin 'ntk148v/vim-horizon'
    Plugin 'rakr/vim-colors-rakr'
    " Plugin 'rakr/vim-one'
    Plugin 'morhetz/gruvbox'
    " Plugin 'ntk148v/vim-horizon'

    " Git
    Plugin 'tpope/vim-fugitive'

    " File support
    Plugin 'cespare/vim-toml'
    Plugin 'lervag/vimtex'
    " Plugin 'aklt/plantuml-syntax'
    Plugin 'LnL7/vim-nix'
    Plugin 'dart-lang/dart-vim-plugin'
    Plugin 'fatih/vim-go'

    " Plugin 'vimwiki/vimwiki'

    " Plugin 'glacambre/firenvim'


    " Snippits
    " Plugin 'SirVer/ultisnips'
    " Plugin 'honza/vim-snippets'

    call vundle#end()            " required}}}

" Completion{{{
    set completeopt+=menuone
    set completeopt+=noselect

    if has('nvim')
        let g:echodoc#enable_at_startup = 1
        let g:echodoc#type = 'virtual'
    endif

    "let g:deoplete#enable_at_startup = 1
    "let g:deoplete#disable_auto_complete = 1

    let b:vcm_tab_complete = 'omni'

    " Close completion window
    autocmd CompleteDone * pclose

    " Lsp options
    " let g:LanguageClient_setOmnifunc = 1
    let g:LanguageClient_hoverPreview = "Never"
    let g:LanguageClient_useVirtualText = "No"
    let g:LanguageClient_loggingFile = "/tmp/lsp.log"
    let g:LanguageClient_loggingLevel = "DEBUG"
    let g:LanguageClient_diagnosticsList = "Disabled"


    " Setup lsp servers
    let g:LanguageClient_serverCommands = {
        \ 'c' : ['clangd'],
        \ 'cpp' : ['clangd'],
        \ 'python' : ['pyls'],
        \ 'go' : ['gopls'],
        \ 'rust' : ['rls'],
        \ 'typescript' : ['tsserver'],
        \ }

    " let g:one_allow_italics = 1
    let g:gruvbox_italic = 1
    colorscheme gruvbox

    " Tab is already in use by completion
    "let g:UltiSnipsExpandTrigger="<c-s>"
    "let g:UltiSnipsJumpForwardTrigger="<c-space>"

    " let g:firenvim_config = { 
    "     \ 'localSettings': {
    "         \ '.*': {
    "             \ 'takeover': 'never',
    "         \ },
    "     \ }
    " \ }}}}
