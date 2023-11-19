" Plugins{{{
  call plug#begin()
    Plug 'neovim/nvim-lspconfig'

    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'

    Plug 'NLKNguyen/papercolor-theme'
    Plug 'lervag/vimtex'

    Plug 'nvim-treesitter/nvim-treesitter'

  call plug#end()

" }}}

" Treesitter {{{
lua << EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = { "lua", "typescript", "java", "tsx" },
    auto_install = false,
    highlight = {
      enable = true,
    },
    indent = {
      enable = true,
    }
  }
EOF
" }}}

" Completion{{{
    set completeopt+=menuone
    set completeopt+=noselect
    "
    " Close completion window
    autocmd CompleteDone * pclose

    colorscheme PaperColor

    "}}}

" Latex {{{
  let g:vimtex_view_method = 'general'
  let g:vimtex_quickfix_open_on_warning = 0

    "}}}
