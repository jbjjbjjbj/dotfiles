" Plugins
    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
    " alternatively, pass a path where Vundle should install plugins
    "call vundle#begin('~/some/path/here')

    " let Vundle manage Vundle, required
    Plugin 'VundleVim/Vundle.vim'

    " Completion
    Plugin 'prabirshrestha/vim-lsp'
    Plugin 'ajh17/VimCompletesMe'
    Plugin 'Shougo/echodoc.vim'

    " Installed through vim-core
    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-repeat'
    Plugin 'tpope/vim-unimpaired'
    Plugin 'tpope/vim-commentary'

    Plugin 'godlygeek/tabular'

    " Editor config
    Plugin 'sgur/vim-editorconfig'

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
    " Plugin 'aklt/plantuml-syntax'
    " Plugin 'LnL7/vim-nix'

    " Plugin 'vimwiki/vimwiki'


    " Snippits
    " Plugin 'SirVer/ultisnips'
    " Plugin 'honza/vim-snippets'

    call vundle#end()            " required

" Completion
    set completeopt+=menuone
    set completeopt+=noselect

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

    " Close completion window
    autocmd CompleteDone * pclose

    " Setup lsp servers
    if executable('clangd')
        au User lsp_setup call lsp#register_server({
        			\ 'name': 'clangd',
        			\ 'cmd': {server_info->['clangd']},
        			\ 'whitelist': ['c', 'cpp'],
        			\ })
    endif
    if executable('pyls')
        au User lsp_setup call lsp#register_server({
        			\ 'name': 'pyls',
        			\ 'cmd': {server_info->['pyls']},
        			\ 'whitelist': ['py', 'python'],
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
        let b:vcm_tab_complete = 'omni'
        set signcolumn=yes
        nmap <buffer> gd <plug>(lsp-definition)
        nmap <buffer> <f2> <plug>(lsp-rename)
        nmap <buffer> zF <plug>(lsp-code-action)
        " refer to doc to add more commands
    endfunction

    augroup lsp_install
        au!
        " call s:on_lsp_buffer_enabled only for languages that has the server registered.
        autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
    augroup END

