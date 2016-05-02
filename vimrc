set nocompatible " be improved always


set number       " Display line numbers
"set relativenumber " Show line numbers relative to current line
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
set nostartofline " don't move cursor to the start of line after (some class of commands).  This includes buffer switching.

syntax on     " Do syntax highlighting

set scrolloff=5  " Auto-scroll up or down to keep context above/below cursor
set nowrap       " turn off word-wrap
set sidescrolloff=5 " Auto-scroll L/R to keep context in view

" complete only up to where possibilities diverge, and list options (ex prompt)
set wildmenu
set wildmode=list:longest,full

set ignorecase   " case insensitive searching
set smartcase    " ignore case in searches unless an uppercase is used.
set incsearch    " search incrementally (move cursor to show nearest match)
set hlsearch     " highlight all results -- turn off temporarily with :nohl
set backspace=indent,eol,start
"set background=dark  " Tell vim the background will be dark so it will set a good color scheme
set cursorcolumn   " Highlight current column
set cursorline     " Highlight current line
"set insertmode     " make vim modeless - bad idea.

set backup    " Make backup files
if !isdirectory($HOME . "/.cache/vimtmp")
    call mkdir($HOME . "/.cache/vimtmp", "p")
endif
set backupdir=$HOME/.cache/vimtmp   " Directory for backup files
set directory=$HOME/.cache/vimtmp   " Directory for swap files


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
set rtp+=$DOTFILESWGH/external/vim/Vundle.vim,$DOTFILESWGH/vim

if filereadable($DOTFILESWGH . "/external/vim/Vundle.vim/README.md")
    call vundle#begin($DOTFILESWGH . "/dotlocal/vim")
    Plugin 'kana/vim-submode'
    Plugin 'tpope/vim-rsi'
    Plugin 'tpope/vim-repeat'
    Plugin 'tpope/vim-surround'
    "Plugin 'tpope/vim-speeddating'
    Plugin 'tpope/vim-markdown'
    Plugin 'tpope/vim-tbone'
    Plugin 'tpope/vim-eunuch'
    Plugin 'tpope/vim-sleuth'
    Plugin 'kana/vim-textobj-user'
    Plugin 'rhysd/vim-textobj-anyblock'
    "Plugin 'tpope/vim-afterimage'
    "Plugin 'tpope/vim-commentary'
    "Plugin 'tpope/vim-abolish'
    "Plugin 'matchit.zip'
    Plugin 'chrisbra/csv.vim'
    " a is alternate
    "Plugin 'a.vim'
    Plugin 'bling/vim-airline'
    Plugin 'Lokaltog/vim-easymotion'
    Plugin 'ap/vim-css-color'
    Plugin 'scrooloose/syntastic'
    Plugin 'pangloss/vim-javascript'
    Plugin 'ervandew/supertab'
    Plugin 'msanders/snipmate.vim'
    "Plugin 'camelcasemotion'
    Plugin 'AndrewRadev/sideways.vim'
    Plugin 'vim-scripts/paredit.vim'
    "Plugin 'luochen1990/rainbow'
    Plugin 'pelodelfuego/vim-swoop'
    "Plugin 'terryma/vim-multiple-cursors'
    call vundle#end()
    " to run the vundle installer, run :PluginInstall

    " Sometimes I can't see the parens.  It might work better when terminal vim
    " supports true color well.
    "Plugin 'kien/rainbow_parentheses.vim'
    "au VimEnter * RainbowParenthesesToggle
    "au Syntax * RainbowParenthesesLoadRound
    "au Syntax * RainbowParenthesesLoadSquare
    "au Syntax * RainbowParenthesesLoadBraces

    let g:airline#extensions#tabline#enabled = 1
    let g:airline_left_sep = '▶'
    let g:airline_right_sep = '◀'

    let g:rainbow_active = 1

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

    let g:submode_always_show_submode = 1
    let g:submode_keyseqs_to_leave = ['<esc>', '<C-g>']
    let g:submode_timeout = 0
    let g:submode_timeoutlen = 9999
    call submode#enter_with('windowing', 'n', '', 'th', '<NOP>')
    call submode#leave_with('windowing', 'n', '', 'e')
    call submode#map('windowing', 'n', '', 'j', '<C-w>w')
    call submode#map('windowing', 'n', '', 'k', '<C-w>W')
    call submode#map('windowing', 'n', '', 's', '<C-w>s')
    call submode#map('windowing', 'n', '', 'v', '<C-w>v')
    call submode#map('windowing', 'n', '', '=', '<C-w>=')
    call submode#map('windowing', 'n', '', 'c', '<C-w>c')
    call submode#map('windowing', 'n', '', 'h', ':vertical resize -5<CR>')
    call submode#map('windowing', 'n', '', 'l', ':vertical resize +5<CR>')
    call submode#map('windowing', 'n', '', 'H', ':resize -5<CR>')
    call submode#map('windowing', 'n', '', 'L', ':resize +5<CR>')
    call submode#map('windowing', 'n', '', 'g', ':tabnew<cr>')
    call submode#map('windowing', 'n', '', 'G', ':tabclose<cr>')
    call submode#map('windowing', 'n', '', 'w', 'gt')
    call submode#map('windowing', 'n', '', 'b', 'gT')

    call submode#enter_with('pager', 'n', '', 'to', '<NOP>')
    call submode#leave_with('pager', 'n', '', 'e')
    call submode#map('pager', 'n', '', 'j', '<C-d>')
    call submode#map('pager', 'n', '', 'k', '<C-u>')
    call submode#map('pager', 'n', '', 'J', '<C-e>')
    call submode#map('pager', 'n', '', 'K', '<C-y>')
    call submode#map('pager', 'n', '', 'sj', '<pagedown>')
    call submode#map('pager', 'n', '', 'sk', '<pageup>')
    call submode#map('pager', 'n', '', 'tic', ':call BufDelOrQuit()<cr>')

    let g:multi_cursor_use_default_mapping=0
    let g:multi_cursor_start_key='<C-e>'
    let g:multi_cursor_next_key='<C-e>'
    let g:multi_cursor_prev_key='<C-o>'
    let g:multi_cursor_skip_key='<C-a>'
    let g:multi_cursor_quit_key='<C-k>'

endif

filetype plugin indent on
autocmd FileType * setlocal formatoptions-=r formatoptions-=o

""""""""""""""""""""""""""" Functions
function! NumBuffers()
    return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
endfunction

function! BufDelOrQuit()
    if NumBuffers() == 1
        :q
    else
        :bd
    endif
endfunction

" switch buffer function, but default to most recent buffer if arguments are empty
function! BufSwitchFunc(buf)
    if a:buf == ""
        execute 'buffer' '#'
    else
        execute 'buffer' a:buf
    endif
endfunction
command -nargs=? -complete=buffer BufSwitch call BufSwitchFunc("<args>")

"set textwidth=80  " set 80 character width
"set colorcolumn=+0 " Color textwidth +0

function! SetColorColumn()
    if &textwidth == 0
        :set colorcolumn=80
    else
        :set colorcolumn=+0
    endif
endfunction

function! SetTextWidth(n)
    :execute "set textwidth=" . a:n
    :call SetColorColumn()
endfunction
command -nargs=1 Width call SetTextWidth("<args>")
command -nargs=1 ColumnWidth call SetTextWidth("<args>")

command -nargs=0 Rot13 normal ggg?G

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

" make C-y default paste, like in emacs
cnoremap <C-y> <C-r>"
inoremap <C-y> <C-r>"
cnoremap <esc>r <C-r>
inoremap <esc>r <C-r>

" o and e maps - o is left/back, e is right/forward
noremap e <nop>
noremap o <nop>
noremap ee e
noremap oe ge
noremap eo o
noremap oo O
noremap ec l
noremap oc h
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
noremap ol <bar>
noremap o<space>l ^
noremap el $
noremap oL g0
noremap eL g$
noremap eb +
noremap ob -
noremap oB ^
noremap eB _
noremap oj <C-o>
noremap ej <C-i>

noremap f ;
noremap F ,
noremap t <nop>
noremap T <nop>
noremap tr "

" I really need to figure out something better here, this binding was
" ill-advised but now I'm used to it
noremap <space>jl J

noremap <space>/s :call Swoop()<CR>
vnoremap <space>/s :call SwoopSelection()<CR>
noremap <space>/a :call SwoopMulti()<CR>
vnoremap <space>/a :call SwoopMultiSelection()<CR>

nnoremap tia :BufSwitch<space>
nnoremap tic :call BufDelOrQuit()<CR>
nnoremap <space>tica :qa<CR>
nnoremap tis :w<CR>
nnoremap <space>tisa :wall<CR>
nnoremap tie :w<CR>:call BufDelOrQuit()<CR>
nnoremap <space>tiea :wqall<CR>
" open file in same directory as current file
" % mean current buffer (like with %s/foo/bar/)
" :p means expand to full path
" :h means head -- ie remove last component
nnoremap tif :e <C-R>=expand("%:p:h") . "/" <CR>
" open file from PWD
nnoremap <space>tifd :e<space>
nnoremap tib :bp<CR>
nnoremap tiw :bn<CR>

nnoremap tst :set wrap!<CR>
nnoremap tsW :set wrapscan!<CR>
nnoremap tsh :nohl<CR>
nnoremap tsH :set hlsearch!<CR>
nnoremap tsr :set relativenumber!<CR>
nnoremap tsi :set ignorecase!<CR>
nnoremap tsa :set autoindent!<CR>

nnoremap tlb :buffers<CR>
nnoremap tlm :marks<CR>
nnoremap tlj :jumps<CR>
nnoremap tlr :registers<CR>

noremap <Space>h <PageDown>
noremap <Space>t <PageUp>
noremap sj <PageDown>
noremap sk <PageUp>
nnoremap <Space> v ggVG

inoremap <C-g> <esc>
noremap <C-g> <esc>
vnoremap <C-g> <esc>
cnoremap <C-g> <C-c>

nnoremap sm m
nnoremap sg `
nnoremap ss s
nnoremap sh :!
vnoremap sh :!
noremap sa :
vnoremap s/ :s/

noremap <space>pc "+p
vnoremap <space>yc "+y
noremap <space>ps "*p
vnoremap <space>ys "*y



" Highlighting
function! SetDarkTheme()
    "colorscheme elflord
    colorscheme wgh-dark
    let g:airline_theme='vairl'
    "call airline#switch_theme('vairl')
    highlight Normal cterm=NONE ctermfg=253 ctermbg=233
    highlight Comment cterm=italic
    highlight ColorColumn ctermbg=17
    highlight CursorLine cterm=NONE ctermbg=235
    highlight CursorColumn ctermbg=235
    highlight Search ctermbg=20 ctermfg=2
endfunction
command -nargs=0 DarkTheme call SetDarkTheme()

function! SetLightTheme()
    colorscheme morning
    "call airline#switch_theme('light')
    highlight Comment cterm=italic
    highlight CursorLine cterm=NONE ctermbg=247
    highlight CursorColumn ctermbg=247
endfunction
command -nargs=0 LightTheme call SetLightTheme()

call SetDarkTheme()

if filereadable($DOTFILESWGH . "/dotlocal/vimrc")
    source $DOTFILESWGH/dotlocal/vimrc
endif

