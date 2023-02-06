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

    lua << EOF
      local on_attach = function(client, bufnr)
        vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
        local bufopts = { noremap=true, silent=true, buffer=bufnr }
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      end

      require('lspconfig')['rust_analyzer'].setup{
        on_attach = on_attach,
        settings = {
          ["rust-analyzer"] = {}
          }
      }

EOF
    "}}}

" Latex {{{
  let g:vimtex_view_method = 'zathura'
  let g:vimtex_quickfix_open_on_warning = 0

    "}}}

" Tree sitter {{{

lua << EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "lua", "vim", "rust", "help" },

  highlight = {
    enable = true,

    disable = { "latex" },
  },
  incremental_selection = {
    enable = true,
  },
}
EOF

" }}}
