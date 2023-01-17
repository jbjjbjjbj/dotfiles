" Plugins{{{
  call plug#begin()
    Plug 'neovim/nvim-lspconfig'
    Plug 'Shougo/echodoc.vim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'

    Plug 'NLKNguyen/papercolor-theme'
    Plug 'LnL7/vim-nix'
    Plug 'fatih/vim-go'
    Plug 'lervag/vimtex'

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
    "}}}

" Latex {{{
  let g:vimtex_view_method = 'zathura'
  let g:vimtex_quickfix_open_on_warning = 0

    "}}}

" Tree sitter {{{

lua << EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "lua", "vim", "rust", "help" },

  auto_install = true,

  highlight = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
  },
}
EOF

" }}}
