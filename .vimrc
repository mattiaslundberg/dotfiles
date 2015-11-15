set nocompatible
set background=dark
set nomodeline

" Vundle stuff
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#rc()
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

" Navigation
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'

" Color
Plugin 'chriskempson/base16-vim.git'
Plugin 'matchit.zip'

Plugin 'bling/vim-airline'
Plugin 'bling/vim-bufferline'

Plugin 'mbbill/undotree'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'vim-scripts/restore_view.vim'
Plugin 'tpope/vim-abolish.git'
Plugin 'gcmt/wildfire.vim'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-commentary'
Plugin 'godlygeek/tabular'
Plugin 'AndrewRadev/linediff.vim'
Plugin 'christoomey/vim-tmux-navigator'

" Completion/snippets
Plugin 'Shougo/neocomplcache'
Plugin 'Shougo/neosnippet'
Plugin 'Shougo/neosnippet-snippets'
Plugin 'honza/vim-snippets'

" Python
Plugin 'klen/python-mode'
Plugin 'python_match.vim'
Plugin 'pythoncomplete'
Plugin 'fisadev/vim-isort'

" Javascript
Plugin 'elzr/vim-json'
Plugin 'groenewege/vim-less'
Plugin 'pangloss/vim-javascript'
Plugin 'briancollins/vim-jst'
Plugin 'kchmck/vim-coffee-script'

" HTML
Plugin 'amirh/HTML-AutoCloseTag'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'gorodinskiy/vim-coloresque'
Plugin 'tpope/vim-haml'
Plugin 'mattn/emmet-vim'

" Other langs
Plugin 'wting/rust.vim'
Plugin 'tpope/vim-markdown'
Plugin 'Puppet-Syntax-Highlighting'
Plugin 'markcornick/vim-vagrant'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'darvelo/vim-systemd'
call vundle#end()
filetype plugin indent on

" General configuration
set nofoldenable
set foldlevel=1
syntax on
set mouse=a
set mousehide
scriptencoding utf-8

if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else
        set clipboard=unnamed
    endif
endif
set shortmess+=filmnrxoOtT
set viewoptions=options,cursor,unix,slash
set virtualedit=onemore
set history=10000
set spell
set hidden
set iskeyword-=.
set iskeyword-=#
set iskeyword-=-
let mapleader = "\<space>"

" Backup files
set backupdir=~/.vimbackup/
set undodir=~/.vimundo/
set viewdir=~/.vimviews/
set backup
set noswapfile
set undofile
set undolevels=1000
set undoreload=10000

" UI and colorscheme
let base16colorspace=256
color base16-default

if has('gui_running')
    set guioptions=a
    set lines=40
    set guifont=Monospace\ 12
else
    set t_Co=256
endif

set tabpagemax=15
set showmode
set cursorline

highlight clear SignColumn
highlight clear LineNr

set backspace=indent,eol,start
set linespace=0
set number
set showmatch
set incsearch
set hlsearch
set winminheight=0
set ignorecase
set smartcase
set wildmenu
set wildmode=list:longest,full
set whichwrap=b,s,h,l,<,>,[,]
set scrolljump=5
set scrolloff=3
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

" Formatting
set nowrap
set autoindent
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow
set pastetoggle=<F12>

autocmd FileType c,cpp,java,go,php,javascript,html,puppet,python,rust,twig,xml,yml,perl autocmd BufWritePre <buffer> call StripTrailingWhitespace()
autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2
autocmd BufNewFile,BufRead * set nofoldenable

autocmd BufNewFile,BufRead *.coffee set filetype=coffee

nnoremap Y y$

vnoremap < <gv
vnoremap > >gv
vnoremap . :normal .<CR>

" Nerd tree
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
nmap <leader>e :NERDTreeToggle<CR>
nmap <leader>fe :NERDTreeFind<CR>

" Snippets
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets,~/.vim/bundle/neosnippet-snippets/neosnippets.vim,~/.vimsnippets'

" Python mode
let g:pymode_indent = 0
let g:pymode_lint_checkers = ['pyflakes', 'pep8']
let g:pymode_options_max_line_length = 120
let g:pymode_lint_options_pep8 =
            \({'max_line_length': g:pymode_options_max_line_length})
let g:pymode_trim_whitespaces = 0
let g:pymode_options = 0
let g:pymode_rope = 0
let g:pymode_lint_ignore = "W0401"

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

" Use c-v and c-x to open ag results in splits
let g:ag_mappings = {
    \ "<C-v>": "<C-W><CR><C-W>H<C-W>b<C-W>J",
    \ "<C-x>": "<C-W><CR><C-W>K" }

" Create continuous split
noremap <silent> <Leader>vs :<C-u>let @z=&so<CR>:set so=0 noscb<CR>:bo vs<CR>Ljzt:setl scb<CR><C-w>p:setl scb<CR>:let &so=@z<CR>

" Completion
let g:acp_enableAtStartup = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_enable_auto_delimiter = 1
let g:neocomplcache_max_list = 15
let g:neocomplcache_force_overwrite_completefunc = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
            \ 'default' : '',
            \ 'vimshell' : $HOME.'/.vimshell_hist',
            \ 'scheme' : $HOME.'/.gosh_completions'
            \ }

let g:neocomplcache_keyword_patterns = {}
let g:neocomplcache_keyword_patterns._ = '\h\w*'

imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
imap <silent><expr><C-k> neosnippet#expandable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ?
            \ "\<C-e>" : "\<Plug>(neosnippet_expand_or_jump)")
smap <TAB> <Right><Plug>(neosnippet_jump_or_expand)

inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()

function! CleverCr()
    if pumvisible()
        if neosnippet#expandable()
            let exp = "\<Plug>(neosnippet_expand)"
            return exp . neocomplcache#close_popup()
        else
            return neocomplcache#close_popup()
        endif
    else
        return "\<CR>"
    endif
endfunction

" <CR> close popup and save indent or expand snippet
imap <expr> <CR> CleverCr()

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()

" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"

let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

" Airline
set laststatus=2
let g:bufferline_echo = 0
let g:airline_left_sep='›'
let g:airline_right_sep='‹'

function! StripTrailingWhitespace()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " do the business:
    %s/\s\+$//e
    " clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" Shell command
function! s:RunShellCommand(cmdline)
    botright new

    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal nobuflisted
    setlocal noswapfile
    setlocal nowrap
    setlocal filetype=shell
    setlocal syntax=shell

    call setline(1, a:cmdline)
    call setline(2, substitute(a:cmdline, '.', '=', 'g'))
    execute 'silent $read !' . escape(a:cmdline, '%#')
    setlocal nomodifiable
    1
endfunction

command! -complete=file -nargs=+ Shell call s:RunShellCommand(<q-args>)

function FixTabs()
    set noexpandtab
    set shiftwidth=4
    set tabstop=4
    set softtabstop=4
    %retab!
endfunction
command FixTabs :call FixTabs()

" Don't restore cursor for git commits
let g:skipview_files = ['COMMIT_EDITMSG']

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

if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
