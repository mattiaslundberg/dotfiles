set nocompatible
set background=dark
set nomodeline

" Vundle stuff
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#rc()
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'altercation/vim-colors-solarized'
Bundle 'spf13/vim-colors'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'spf13/vim-autoclose'
Bundle 'ctrlpvim/ctrlp.vim'
Bundle 'tacahiroy/ctrlp-funky'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'vim-scripts/sessionman.vim'
Bundle 'matchit.zip'
Bundle 'bling/vim-airline'
Bundle 'bling/vim-bufferline'
" Bundle 'Lokaltog/vim-easymotion'
Bundle 'jistr/vim-nerdtree-tabs'
" Bundle 'flazz/vim-colorschemes'
Bundle 'mbbill/undotree'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'vim-scripts/restore_view.vim'
Bundle 'mhinz/vim-signify'
Bundle 'tpope/vim-abolish.git'
Bundle 'osyo-manga/vim-over'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'gcmt/wildfire.vim'
Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-fugitive'
Bundle 'mattn/webapi-vim'
" "Bundle 'mattn/gist-vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-commentary'
Bundle 'godlygeek/tabular'

" Completion
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/neosnippet'
Bundle 'Shougo/neosnippet-snippets'
Bundle 'honza/vim-snippets'

" Python
Bundle 'klen/python-mode'
Bundle 'python_match.vim'
Bundle 'pythoncomplete'

" Javascript
Bundle 'elzr/vim-json'
Bundle 'groenewege/vim-less'
Bundle 'pangloss/vim-javascript'
Bundle 'briancollins/vim-jst'
Bundle 'kchmck/vim-coffee-script'

" HTML
Bundle 'amirh/HTML-AutoCloseTag'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'gorodinskiy/vim-coloresque'
Bundle 'tpope/vim-haml'
Bundle 'mattn/emmet-vim'

" MISC
Bundle 'wting/rust.vim'
Bundle 'tpope/vim-markdown'
Bundle 'spf13/vim-preview'
Bundle 'tpope/vim-cucumber'
Bundle 'cespare/vim-toml'
Bundle 'quentindecock/vim-cucumber-align-pipes'
Bundle 'Puppet-Syntax-Highlighting'
Bundle 'markcornick/vim-vagrant'
Bundle 'ekalinin/Dockerfile.vim'
Bundle 'thanthese/Tortoise-Typing'
call vundle#end()
filetype plugin indent on

" General configuration
set nofoldenable
syntax on					" Syntax highlighting
set mouse=a					" Automatically enable mouse usage
set mousehide				" Hide the mouse cursor while typing
scriptencoding utf-8

if has('clipboard')
	if has('unnamedplus')  " When possible use + register for copy-paste
		set clipboard=unnamed,unnamedplus
	else		 " On mac and Windows, use * register for copy-paste
		set clipboard=unnamed
	endif
endif
set shortmess+=filmnrxoOtT			" Abbrev. of messages (avoids 'hit enter')
set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
set virtualedit=onemore				" Allow for cursor beyond last character
set history=1000					" Store a ton of history (default is 20)
set spell							" Spell checking on
set hidden							" Allow buffer switching without saving
set iskeyword-=.					" '.' is an end of word designator
set iskeyword-=#					" '#' is an end of word designator
set iskeyword-=-					" '-' is an end of word designator

" Backup files
set backupdir=~/.vimbackup/
set undodir=~/.vimundo/
set viewdir=~/.vimviews/
set backup
set noswapfile
if has('persistent_undo')
	set undofile				" So is persistent undo ...
	set undolevels=1000			" Maximum number of changes that can be undone
	set undoreload=10000		" Maximum number lines to save for undo on a buffer reload
endif

" UI and colorscheme
let g:solarized_termcolors=256
let g:solarized_termtrans=1
let g:solarized_contrast="normal"
let g:solarized_visibility="normal"
color solarized
set tabpagemax=15
set showmode					" Display the current mode

set cursorline					" Highlight current line

highlight clear SignColumn		" SignColumn should match background
highlight clear LineNr			" Current line number row will have same background color in relative mode

if has('cmdline_info')
	set ruler					" Show the ruler
	set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
	set showcmd
endif
if has('statusline')
	set laststatus=2

	" Broken down into easily includeable segments
	set statusline=%<%f\					 " Filename
	set statusline+=%w%h%m%r				 " Options
	set statusline+=%{fugitive#statusline()} " Git Hotness
	set statusline+=\ [%{&ff}/%Y]			 " Filetype
	set statusline+=\ [%{getcwd()}]			 " Current dir
	set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

set backspace=indent,eol,start	" Backspace for dummies
set linespace=0					" No extra spaces between rows
set nu							" Line numbers on
set showmatch					" Show matching brackets/parenthesis
set incsearch					" Find as you type search
set hlsearch					" Highlight search terms
set winminheight=0				" Windows can be 0 line high
set ignorecase					" Case insensitive search
set smartcase					" Case sensitive when uc present
set wildmenu					" Show list instead of just completing
set wildmode=list:longest,full	" Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]	" Backspace and cursor keys wrap too
set scrolljump=5				" Lines to scroll when cursor leaves screen
set scrolloff=3					" Minimum lines to keep above and below cursor
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" Formatting
set nowrap						" Do not wrap long lines
set autoindent					" Indent at the same levelof the previous line
set noexpandtab
set shiftwidth=4				" Use indents of 4 spaces
set tabstop=4					" An indentation every four columns
set softtabstop=4				" Let backspace delete indent
set nojoinspaces				" Prevents inserting two spaces after punctuation on a join (J)
set splitright					" Puts new vsplit windows to the right of the current
set splitbelow					" Puts new split windows to the bottom of the current
set pastetoggle=<F12>			" pastetoggle (sane indentation on pastes)
autocmd FileType c,cpp,java,go,php,javascript,html,puppet,python,rust,twig,xml,yml,perl autocmd BufWritePre <buffer> call StripTrailingWhitespace()
autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType python setl shiftwidth=4 tabstop=4 noexpandtab softtabstop=4
autocmd BufRead set nofoldenable

autocmd BufNewFile,BufRead *.coffee set filetype=coffee

" Workaround vim-commentary for Haskell
autocmd FileType haskell setlocal commentstring=--\ %s
" Workaround broken colour highlighting in Haskell
autocmd FileType haskell,rust setlocal nospell

if has("user_commands")
	command! -bang -nargs=* -complete=file E e<bang> <args>
	command! -bang -nargs=* -complete=file W w<bang> <args>
	command! -bang -nargs=* -complete=file Wq wq<bang> <args>
	command! -bang -nargs=* -complete=file WQ wq<bang> <args>
	command! -bang Wa wa<bang>
	command! -bang WA wa<bang>
	command! -bang Q q<bang>
	command! -bang QA qa<bang>
	command! -bang Qa qa<bang>
endif

nnoremap Y y$

vnoremap < <gv
vnoremap > >gv
vnoremap . :normal .<CR>

" Plugin configurations
"let g:NERDShutUp=1
nmap <Leader>ac <Plug>ToggleAutoCloseMappings
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']

let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
let g:neosnippet#snippets_directory='~/.vimsnippets'

let g:pymode_lint_checkers = ['pyflakes']
let g:pymode_trim_whitespaces = 0
let g:pymode_options = 0
let g:pymode_rope = 0

if isdirectory(expand("~/.vim/bundle/ctrlp.vim/"))
	let g:ctrlp_working_path_mode = 'ra'
	nnoremap <silent> <D-t> :CtrlP<CR>
	nnoremap <silent> <D-r> :CtrlPMRU<CR>
	let g:ctrlp_custom_ignore = {
				\ 'dir':  '\.git$\|\.hg$\|\.svn$',
				\ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

	let s:ctrlp_fallback = 'ack %s --nocolor -f'
	let g:ctrlp_user_command = {
				\ 'types': {
				\ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
				\ 2: ['.hg', 'hg --cwd %s locate -I .'],
				\ },
				\ 'fallback': s:ctrlp_fallback
				\ }

	if isdirectory(expand("~/.vim/bundle/ctrlp-funky/"))
		" CtrlP extensions
		let g:ctrlp_extensions = ['funky']

		"funky
		nnoremap <Leader>fu :CtrlPFunky<Cr>
	endif
endif

nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>ge :Gedit<CR>
" Mnemonic _i_nteractive
nnoremap <silent> <leader>gi :Git add -p %<CR>
nnoremap <silent> <leader>gg :SignifyToggle<CR>

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

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
	let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns._ = '\h\w*'

imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
imap <silent><expr><C-k> neosnippet#expandable() ?
			\ "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ?
			\ "\<C-e>" : "\<Plug>(neosnippet_expand_or_jump)")
smap <TAB> <Right><Plug>(neosnippet_jump_or_expand)

inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()
"inoremap <expr><CR> neocomplcache#complete_common_string()

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

" <CR>: close popup
" <s-CR>: close popup and save indent.
inoremap <expr><s-CR> pumvisible() ? neocomplcache#close_popup()"\<CR>" : "\<CR>"
"inoremap <expr><CR> pumvisible() ? neocomplcache#close_popup() : "\<CR>"

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()

" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"

let g:pymode_indent = 0
let g:pymode_lint_checkers = ['pyflakes']
let g:pymode_trim_whitespaces = 0
let g:pymode_options = 0
let g:pymode_rope = 0

let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

let g:airline_theme = 'solarized'
let g:airline_left_sep='›'	" Slightly fancier than '>'
let g:airline_right_sep='‹' " Slightly fancier than '<'

if has('gui_running')
	set guioptions-=T
	set guioptions-=l
	set guioptions-=r
	set lines=40
	set guifont=Monospace\ 13
else
	if &term == 'xterm' || &term == 'screen'
		set t_Co=256			" Enable 256 colors to stop the CSApprox warning and make xterm vim shine
	endif
	"set term=builtin_ansi		 " Make arrow and other keys work
endif

" Strip whitespace {
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

" Shell command {
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

function FixTabs()
	set noexpandtab
	set shiftwidth=4
	set tabstop=4
	set softtabstop=4
	%retab!
endfunction
command FixTabs :call FixTabs()

command! -complete=file -nargs=+ Shell call s:RunShellCommand(<q-args>)

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Custom keymappings
let mapleader = ","
imap jk <Esc>
imap kj <Esc>
map <Leader>w :w<CR>
map <Leader>x :x<CR>
map <Leader>qq :qa!<CR>
map <Leader>qw :wqa<CR>
if filereadable(expand("~/.vimrc.local"))
	source ~/.vimrc.local
endif
