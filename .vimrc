filetype on
filetype indent on
filetype plugin on
 
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
  Bundle 'gmarik/vundle'
  Bundle 'vim-ruby/vim-ruby'
  Bundle 'tpope/vim-rails'
  Bundle 'scrooloose/nerdtree'
  Bundle 'Valloric/YouCompleteMe'
  Bundle 'tpope/vim-endwise'
  Bundle 'mattn/gist-vim'
  Bundle 'chriskempson/base16-vim'
  Bundle 'wincent/Command-T'
" All of your Plugins must be added before the following line
call vundle#end()            " required
 
set background=dark
colors railscasts
"colorscheme base16-railscasts
"base16-tomorrow
 
:set number 
:syn on
 
:set enc=utf-8
 
:set tabstop=2
:set shiftwidth=2
:set expandtab
:set nocompatible
:set nobackup
:set nowritebackup
:set noswapfile
 
:tab sball
:set switchbuf=usetab
 
:set foldmethod=indent
:set foldlevel=100
 
:set formatoptions=1
:set lbr
 
:set ruler
