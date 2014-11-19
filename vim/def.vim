set nocompatible

set t_Co=256

filetype off " required for vundle, supposedly

set rtp+=$DOTFILESDIR/external/vim/Vundle.vim

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

" source default vimrc
source $DOTFILESDIR/vimrc

