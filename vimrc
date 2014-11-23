set nocompatible " be improved always

colorscheme elflord

set number       " Display line numbers
set relativenumber " Show line numbers relative to current line
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
set showcmd      " show prefix keys as they are typed
set laststatus=2 " always show statusbar
set hidden       " allow switching from dirty buffers without saving or killing changes

syntax on     " Do syntax highlighting

set scrolloff=5  " Auto-scroll up or down to keep context above/below cursor
set nowrap       " turn off word-wrap
set sidescrolloff=5 " Auto-scroll L/R to keep context in view
set textwidth=0  " set 80 character width
set colorcolumn=+80 " Color column 80

" complete only up to where possibilities diverge, and list options (ex prompt)
set wildmenu
set wildmode=list:longest,full

set ignorecase   " case insensitive searching
set smartcase    " ignore case in searches unless an uppercase is used.
set incsearch    " search incrementally (move cursor to show nearest match)
set hlsearch     " highlight all results -- turn off temporarily with :nohl
set backspace=indent,eol,start
"set background=dark  " Tell vim the background will be dark so it will set a good color scheme
"set cursorcolumn   " Highlight current column
"set cursorline     " Highlight current line
"set insertmode     " make vim modeless!  I might check this out later...

set backup    " Make backup files
set backupdir=$DOTFILESLOCALDIR/vimtmp   " Directory for backup files
set directory=$DOTFILESLOCALDIR/vimtmp   " Directory for swap files


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

set t_Co=256
filetype off " required for vundle, supposedly
set rtp+=$DOTFILESDIR/external/vim/Vundle.vim

if filereadable($DOTFILESDIR . "/external/vim/Vundle.vim/README.md")
    call vundle#begin($DOTFILESLOCALDIR . "/vim")
    Plugin 'tpope/vim-rsi'
    Plugin 'tpope/vim-repeat'
    Plugin 'tpope/vim-surround'
    "Plugin 'tpope/vim-speeddating'
    Plugin 'tpope/vim-markdown'
    Plugin 'tpope/vim-tbone'
    Plugin 'tpope/vim-eunuch'
    "Plugin 'tpope/vim-afterimage'
    "Plugin 'tpope/vim-commentary'
    "Plugin 'tpope/vim-abolish'
    Plugin 'matchit.zip'
    " a is alternate
    Plugin 'a.vim'
    Plugin 'bling/vim-airline'
    Plugin 'Lokaltog/vim-easymotion'
    Plugin 'scrooloose/syntastic'
    Plugin 'pangloss/vim-javascript'
    Plugin 'ervandew/supertab'
    Plugin 'msanders/snipmate.vim'
    Plugin 'camelcasemotion'
    Plugin 'AndrewRadev/sideways.vim'
    call vundle#end()
    " to run the vundle installer, run :PluginInstall

    let g:airline#extensions#tabline#enabled = 1

    filetype plugin indent on

    " easymotion -- like acejump
    nmap gc <Plug>(easymotion-s2)
    nmap gtc <Plug>(easymotion-t2)
    map  g/ <Plug>(easymotion-sn)
    omap g/ <Plug>(easymotion-tn)
    "map <Leader>l <Plug>(easymotion-lineforward)
    "map <Leader>j <Plug>(easymotion-j)
    "map <Leader>k <Plug>(easymotion-k)
    "map <Leader>h <Plug>(easymotion-linebackward)
    let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion
    let g:EasyMotion_smartcase = 1
    " these can be bound maybe...
    "<Plug>(easymotion-next)
    "<Plug>(easymotion-prev)
endif

filetype plugin indent on

""""""""""""""""""""""""""" Key mappings
" map, noremap, etc work in normal, visual+select, operator-pending modes...
" vmap vnoremap... vork in visual+select
" n is normal only, o is operator only, x is visual only, s is select only
set timeout timeoutlen=10000 ttimeoutlen=50
set <f13>=jk
set <f14>=kj
inoremap <f13> <esc>
inoremap <f14> <esc>
noremap - :
inoremap <C-\> <C-o>
inoremap <M-c> <C-o>:
inoremap <esc>c <C-o>:
inoremap <M-x> <C-o>:
inoremap <esc>x <C-o>:
noremap <M-c> :
noremap <esc>c :
noremap <M-x> :
noremap <esc>x :

" o and e maps - o is left/back, e is right/forward
noremap e <nop>
noremap o <nop>
noremap ee e
noremap oe ge
noremap eo o
noremap oo O
noremap eE E
noremap oE gE
noremap ea a
noremap oE gE
" ghetto forward/backward arg
noremap ea f,w
noremap oa F,;w
noremap eA :SidewaysRight<cr>
noremap oA :SidewaysLeft<cr>
omap aa <Plug>SidewaysArgumentTextobjA
xmap aa <Plug>SidewaysArgumentTextobjA
omap ia <Plug>SidewaysArgumentTextobjI
xmap ia <Plug>SidewaysArgumentTextobjI
map ew <Plug>CamelCaseMotion_w
map ow <Plug>CamelCaseMotion_b
map eW <Plug>CamelCaseMotion_e
map oW <Plug>CamelCaseMotion_ge
onoremap ilw <Plug>CamelCaseMotion_iw
vnoremap ilw <Plug>CamelCaseMotion_iw
noremap es )
noremap os (
noremap ep }
noremap op {
noremap eh /
noremap oh ?
noremap ef f
noremap of F
noremap et t
noremap ot T
noremap em ;
noremap om ,
noremap en n
noremap on N
noremap ol ^
noremap o<space>l 0
noremap el $
noremap oL g0
noremap eL g$
noremap eb +
noremap ob -
noremap oB ^
noremap eB _

noremap t <nop>
noremap tt t
noremap tT T
noremap tr "
" window keys
nnoremap thic :bdelete<CR>
nnoremap thiac :qa<CR>
nnoremap this :w<CR>
nnoremap thias :wall<CR>
nnoremap thie :w<CR>:bdelete<CR>
nnoremap thiae :wqall<CR>
nnoremap thif :e<space>

nnoremap thn <C-w>w
nnoremap thp <C-w>W
nnoremap thb :bp<CR>
nnoremap thw :bn<CR>
nnoremap tha :buffer<space>
nnoremap thq :bdelete<CR>

" toggle highlighting off after search
nnoremap tst :set wrap!<CR>
nnoremap tsh :nohl<CR>
nnoremap tsH :set hlsearch!<CR>
nnoremap tsr :set relativenumber!<CR>
nnoremap tsi :set ignorecase!<CR>
nnoremap tsa :set autoindent!<CR>

nnoremap tlb :buffers<CR>
nnoremap tlm :marks<CR>

noremap <Space>h <PageDown>
noremap <Space>t <PageUp>
noremap <Space>j <PageDown>
noremap <Space>k <PageUp>
nnoremap <Space> v ggVG

inoremap <C-g> <esc>
noremap <C-g> <esc>
vnoremap <C-g> <esc>
cnoremap <C-g> <esc>

nnoremap sm m
nnoremap sM :marks<cr>
nnoremap sg `
nnoremap sG '
nnoremap ss s
nnoremap sh :!
vnoremap sh :!
vnoremap s/ :s/

noremap gp "+p
vnoremap gy "+y


if filereadable($DOTFILESLOCALDIR . "/vimrc")
    source $DOTFILESLOCALDIR/vimrc
endif

