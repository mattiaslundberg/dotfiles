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
Plug 'ctrlpvim/ctrlp.vim'
Plug 'gavinbeatty/dragvisuals.vim'

" Color
Plug 'chriskempson/base16-vim'

Plug 'vim-scripts/restore_view.vim'
Plug 'benekastah/neomake'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'christoomey/vim-tmux-navigator'
Plug 'thirtythreeforty/lessspace.vim'
Plug 'tweekmonster/braceless.vim'

" Other langs
Plug 'tpope/vim-markdown'
Plug 'markcornick/vim-vagrant'
Plug 'hashivim/vim-terraform'
Plug 'pearofducks/ansible-vim'

Plug 'sheerun/vim-polyglot'
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
color base16-flat
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

nnoremap <leader>\ :let @/ = ""<CR>

" Make more consistent with spacemacs
noremap <leader>fs :w<CR>
noremap <leader>w <C-w>
noremap <leader>qq :wqall!<CR>
noremap ,c :wqall!<CR>
noremap ,k :qall!<CR>

vnoremap < <gv
vnoremap > >gv
vnoremap . :normal .<CR>
vmap  <expr>  <LEFT>   DVB_Drag('left')
vmap  <expr>  <RIGHT>  DVB_Drag('right')
vmap  <expr>  <DOWN>   DVB_Drag('down')
vmap  <expr>  <UP>     DVB_Drag('up')
vmap  <expr>  D        DVB_Duplicate()

if has("mac")
    nnoremap <silent> <C-=> :TmuxNavigateLeft<cr>
endif

" Nerd tree
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
nmap <leader>e :NERDTreeToggle<CR>
nmap <leader>fe :NERDTreeFind<CR>

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

let g:ctrlp_user_command = 'ag -l --nocolor -f -g "" %s'
let g:ctrlp_use_caching = 0

" Ag command
command! -nargs=* -complete=file Ag Grepper -tool ag -query <args>
nnoremap <leader>a :Ag <C-r><c-w><cr>

" Find all files in path
set path=$PWD/**

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
