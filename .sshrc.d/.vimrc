set nocompatible
set background=dark

if has('folding')
	set nofoldenable
endif

if has('syntax')
	if !exists('g:syntax_on')
		syntax enable
	endif
	syntax on
endif
set showmode
set cursorline
set autoindent
set smarttab
set backspace=indent,eol,start
set number
set relativenumber

cmap w!! w !sudo tee % >/dev/null

map <space>w :w<CR>
map <space>x :x<CR>
map <space>qq :qa!<CR>
map <space>qw :wqa<CR>
