" Plugins{{{
  call plug#begin()
    Plug 'williamboman/mason.nvim'
    Plug 'williamboman/mason-lspconfig.nvim'
    Plug 'neovim/nvim-lspconfig'

    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-commentary'

    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/nvim-cmp'

    Plug 'nvim-treesitter/nvim-treesitter'

    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim', { 'branch': '0.1.x' }

    Plug 'NLKNguyen/papercolor-theme'
    Plug 'lervag/vimtex'
  call plug#end()

" }}}

colorscheme PaperColor

" Treesitter {{{
lua <<EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = { "lua", "typescript", "java", "tsx" },
    auto_install = false,
    highlight = {
      enable = true,
    },
    indent = {
      enable = true,
    },
    incremental_selection = {
      enable = true,
    },
  }
EOF
" }}}

" Telescope {{{
lua <<EOF
  local builtin = require('telescope.builtin')
  vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
  vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
EOF
" }}}

" Completion{{{
    set completeopt+=menuone
    set completeopt+=noselect

    " Close completion window
    autocmd CompleteDone * pclose

    "}}}

" Latex {{{
  let g:vimtex_view_method = 'general'
  let g:vimtex_quickfix_open_on_warning = 0

    "}}}

" LSP {{{

lua <<EOF
    require("mason").setup()
    require("mason-lspconfig").setup()

    local lspconfig = require("lspconfig")

    local typescript_formatter = {
      formatCommand = "prettier --stdin --stdin-filepath '${INPUT}' ${--range-start:charStart} ${--range-end:charEnd} ${--tab-width:tabSize} ${--use-tabs:!insertSpaces}",
      formatCanRange = true,
      formatStdin = true,
      rootMarkers = {
        '.prettierrc',
        '.prettierrc.json',
        '.prettierrc.js',
        '.prettierrc.yml',
        '.prettierrc.yaml',
        '.prettierrc.json5',
        '.prettierrc.mjs',
        '.prettierrc.cjs',
        '.prettierrc.toml',
      },
    }

    lspconfig.omnisharp.setup {}
    lspconfig.tsserver.setup {}
    lspconfig.jdtls.setup {}
    lspconfig.pyright.setup {}
    lspconfig.eslint.setup {}
    lspconfig.sourcekit.setup {}
    lspconfig.efm.setup {
      settings = {
        rootMarkers = {".git/"},
        languages = {
          typescript = typescript_formatter,
          typescriptreact = typescript_formatter,
        },
      },
      init_options = {documentFormatting = true},
      filetypes = { "typescript","typescriptreact" },
    }

    local cmp = require'cmp'

    cmp.setup({
      mapping = cmp.mapping.preset.insert({
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort()
      }),
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
      }, {
        {
            name = 'buffer',
            option = {
              get_bufnrs = function()
                local bufs = {}
                for _, win in ipairs(vim.api.nvim_list_wins()) do
                  bufs[vim.api.nvim_win_get_buf(win)] = true
                end
                return vim.tbl_keys(bufs)
              end
              },
        },
        })
    })

    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<leader>a', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', '<leader>d', vim.diagnostic.disable, opts)
        vim.keymap.set('n', '<leader>he', vim.diagnostic.open_float, opts)
        vim.keymap.set('n', '<leader>hh', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', '<leader>q', vim.diagnostic.setqflist, opts)

        vim.api.nvim_set_hl(0, '@lsp.type.function', {})
      end,
    })

EOF
" }}}
