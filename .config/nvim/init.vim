" Plugins
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source ~/.config/nvim/init.vim
endif

call plug#begin('~/.config/nvim/plugged')
" Global configuration
Plug 'editorconfig/editorconfig-vim'

" Navigation
Plug 'mhinz/vim-grepper'
Plug 'milkypostman/vim-togglelist'
Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle', 'NERDTreeFind']}
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'mattiaslundberg/ctrlp.vim', {'branch': 'mlfixes'}
Plug 'mattiaslundberg/ctrlp-funky', {'branch': 'mlfixes'}

" Color
Plug 'chriskempson/base16-vim'

Plug 'vim-scripts/restore_view.vim'
Plug 'benekastah/neomake'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdcommenter'
Plug 'AndrewRadev/linediff.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'thirtythreeforty/lessspace.vim'
Plug 'tweekmonster/braceless.vim'

" Completion/snippets
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'honza/vim-snippets'

" Python
Plug 'fisadev/vim-isort'

" Javascript
Plug 'elzr/vim-json'
Plug 'pangloss/vim-javascript'
Plug 'briancollins/vim-jst'
Plug 'kchmck/vim-coffee-script'
Plug 'mxw/vim-jsx'

" HTML
Plug 'matchit.zip'
Plug 'amirh/HTML-AutoCloseTag'
Plug 'hail2u/vim-css3-syntax'
Plug 'groenewege/vim-less'
Plug 'mattn/emmet-vim'

" Other langs
Plug 'pearofducks/ansible-vim'
Plug 'wting/rust.vim'
Plug 'tpope/vim-markdown'
Plug 'Puppet-Syntax-Highlighting'
Plug 'markcornick/vim-vagrant'
Plug 'ekalinin/Dockerfile.vim'
Plug 'darvelo/vim-systemd'
Plug 'elixir-lang/vim-elixir'
call plug#end()

" General configuration
set background=dark
set nomodeline
set nofoldenable
set foldlevel=1
syntax on
set gdefault
scriptencoding utf-8

set clipboard=unnamedplus
set shortmess+=filmnrxoOtT
set viewoptions=options,cursor,unix,slash
set virtualedit=onemore
set history=10000
set hidden
set iskeyword-=.
set iskeyword-=#
set iskeyword-=-
let mapleader = "\<space>"

set nobackup
set noswapfile
set undofile
set undolevels=1000
set undoreload=10000

" UI and colorscheme
let base16colorspace=256
color base16-default-dark
set t_Co=256

set tabpagemax=15
set noshowmode
set cursorline

highlight clear SignColumn
highlight clear LineNr

set linespace=0
set number
set relativenumber
set showmatch
set incsearch
set hlsearch
set ignorecase
set smartcase
set wildmode=list:longest,full
set whichwrap=b,s,h,l,<,>,[,]
set scrolljump=5
set scrolloff=3
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

" Formatting
set nowrap
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow
set pastetoggle=<F12>

augroup customcmds
autocmd FileType python BracelessEnable +indent
autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2 tabstop=2
autocmd BufNewFile,BufRead * set nofoldenable
autocmd BufWritePost * Neomake

autocmd BufNewFile,BufRead *.coffee set filetype=coffee
autocmd BufNewFile,BufRead *.jsx,*.js set filetype=javascript.jsx

"Save marks for last file of type
autocmd BufLeave *.css,*.less,*scss normal! mC
autocmd BufLeave *models* normal! mM
autocmd BufLeave *api.py normal! mA
autocmd BufLeave *views.py normal! mV
autocmd BufLeave *test.*,*test_* normal! mT
autocmd BufLeave *.html normal! mH
autocmd BufLeave *.jsx,*.js normal! mJ
augroup end

nnoremap Y y$

vnoremap < <gv
vnoremap > >gv
vnoremap . :normal .<CR>

if has("mac")
    nnoremap <silent> <C-=> :TmuxNavigateLeft<cr>
endif

" Nerd tree
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
nmap <leader>e :NERDTreeToggle<CR>
nmap <leader>fe :NERDTreeFind<CR>

" Snippets
let g:neosnippet#snippets_directory='~/.config/nvim/bundle/vim-snippets/snippets,~/.config/nvim/bundle/neosnippet-snippets/neosnippets.vim,~/.vimsnippets'

" Neomake
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_jsx_enabled_makers = ['eslint']
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_airline = 0

" CtrlP
let g:ctrlp_working_path_mode = 'ra'
nnoremap <silent> <D-t> :CtrlP<CR>
nnoremap <silent> <D-r> :CtrlPMRU<CR>
let g:ctrlp_custom_ignore = {
            \ 'dir':  '\.git$\|\.hg$\|\.svn$\|venv$',
            \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

" funky
let g:ctrlp_extensions = ['funky']
nnoremap <Leader>fu :CtrlPFunky<Cr>

let g:ctrlp_user_command = 'ag -l --nocolor -f -g "" %s'
let g:ctrlp_use_caching = 0

" Ag command
command! -nargs=* -complete=file Ag Grepper -tool ag -query <args>
nnoremap <leader>a :Ag <C-r><c-w><cr>

" Toogle quickfix listg:toggle_list_no_mappings
let g:toggle_list_no_mappings = 1
nmap <script> <silent> <leader>k :call ToggleQuickfixList()<CR>
nmap <script> <silent> <leader>l :call ToggleLocationList()<CR>

" Completion
let g:acp_enableAtStartup = 0
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#enable_camel_case = 1
let g:deoplete#max_list = 15
let g:deoplete#enable_auto_delimiter = 1

" Define dictionary.
let g:deoplete#sources#dictionary#dictionaries = {
            \ 'default' : '',
            \ 'vimshell' : $HOME.'/.vimshell_hist',
            \ 'scheme' : $HOME.'/.gosh_completions'
            \ }

imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
imap <silent><expr><C-k> neosnippet#expandable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ?
            \ "\<C-e>" : "\<Plug>(neosnippet_expand_or_jump)")
smap <TAB> <Right><Plug>(neosnippet_jump_or_expand)

function! CleverCr()
    if pumvisible()
        if neosnippet#expandable()
            let exp = "\<Plug>(neosnippet_expand)"
            return exp . deoplete#close_popup()
        else
            return deoplete#close_popup()
        endif
    else
        return "\<CR>"
    endif
endfunction

" <CR> close popup and save indent or expand snippet
imap <expr> <CR> CleverCr()

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> deoplete#close_popup()

" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"

set laststatus=2
set statusline=%<%F
set statusline+=%=
set statusline+=[%{strlen(&fenc)?&fenc:'none'},%{&ff}]
set statusline+=%h%w%m%r%y\ %l/%L

" Don't restore cursor for git commits
let g:skipview_files = ['COMMIT_EDITMSG', 'PULLREQ_EDITMSG']

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

"Disable ex mode
nnoremap Q <nop>

" Custom keymappings
map <Leader>w :w<CR>
map <Leader>x :x<CR>
map <Leader>qq :qa!<CR>
map <Leader>qw :wqa<CR>

nmap < <<
nmap > >>

if !empty(glob("project.vim"))
    source project.vim
endif
