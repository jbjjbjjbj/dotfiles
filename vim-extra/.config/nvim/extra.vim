" Plugins{{{
  call plug#begin()
    Plug 'neovim/nvim-lspconfig'
    Plug 'Shougo/echodoc.vim'

    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'

    Plug 'NLKNguyen/papercolor-theme'
    Plug 'LnL7/vim-nix'
    Plug 'fatih/vim-go'

  call plug#end()

" }}}

" Completion{{{
    set completeopt+=menuone
    set completeopt+=noselect

    if has('nvim')
        let g:echodoc#enable_at_startup = 1
        let g:echodoc#type = 'virtual'
    endif

    " Close completion window
    autocmd CompleteDone * pclose

    colorscheme PaperColor
    "
" Latex
    let g:vimtex_quickfix_open_on_warning = 0
