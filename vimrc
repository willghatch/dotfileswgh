


colorscheme elflord


set number       " Display line numbers
" indenting
set autoindent   " Use indent from previous line when starting a new line
set copyindent   " Use tab/space format from previous line, even with expandtab
"set cindent      " use C-style indenting automatically
"set smartindent  " try to auto-indent... apparently not as good as cindent.
set smarttab     " use shiftwidth instead of tabstop or softtabstop to determine num spaces for tab
set tabstop=8    " Width of actual tabs, I think
set softtabstop=4 " ?
set shiftwidth=4 " How many spaces to indent on a tab
set expandtab    " automaticall turn tabs to spaces

syntax on     " Do syntax highlighting

set scrolloff=5  " Auto-scroll up or down to keep context above/below cursor
set nowrap       " turn off word-wrap
set sidescrolloff=5 " Auto-scroll L/R to keep context in view
set textwidth=0  " set 80 character width
set colorcolumn=+80 " Color column 80

"set smartcase    " ignore case in searches unless an uppercase is used.
set backspace=indent,eol,start
"set background=dark  " Tell vim the background will be dark so it will set a good color scheme
"set cursorcolumn   " Highlight current column
"set cursorline     " Highlight current line
"set insertmode     " make vim modeless!  I might check this out later...

set backup    " Make backup files
set backupdir=~/.vimtmp   " Directory for backup files
set directory=~/.vimtmp   " Directory for swap files


""" Command window stuff
"set cmdheight=1       " num lines for the command-line
"set cmdwinheight=7    " num lines for the command-line window

"set complete=.w,b,u,t,i     " what buffers to scan for autocomplete with ^n and ^p
" there are several complete<x> options... I should read about them sometime.a
set confirm    " confirm exit on unsaved files, rather than failing if no ! is used.

"""" For X11 stuff
set mouse=a     " Allow mouse stuff in all modes
"set clipboard=unnamedplus      " Use '+' register (X11 clipboard) for yanking/putting. 
                                "Alternatively, unnamed uses '*', which is the middle click clipboard.
filetype plugin on




""""""""""""""""""""""""""" Key mappings
inoremap jj <esc>
inoremap <C-@> _
inoremap <C-h> <esc>
nnoremap - :








source ~/.vimrc.local

