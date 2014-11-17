set nocompatible
filetype off " required for vundle, supposedly

set rtp+=$DOTFILESDIR/external/vim/Vundle.vim

call vundle#begin()
Plugin 'tpope/vim-rsi'
call vundle#end()
" to run the vundle installer, run :PluginInstall

filetype plugin indent on

" source default vimrc
source $DOTFILESDIR/vimrc

