set nocompatible
set background=dark

set nofoldenable
syntax on
set showmode
set cursorline
set backspace=indent,eol,start
set nu

cmap w!! w !sudo tee % >/dev/null

let mapleader = ","
imap jk <Esc>
imap kj <Esc>
map <Leader>w :w<CR>
map <Leader>x :x<CR>
map <Leader>qq :qa!<CR>
map <Leader>qw :wqa<CR>
