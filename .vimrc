" for Plugins and Vundle
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'


" word wrap
:set wrap
:set linebreak
:set textwidth=0
:set wrapmargin=0
:set formatoptions+=l

syntax enable
filetype indent on

" Latex stuff
let g:tex_flavor = "latex"
:map  \ll :! cd %:p:h ; pdflatex %:t <CR>
:map  \lb :! biber %:r  <CR>

" Statusline
set laststatus=2
set statusline=#%n\ %<%F%m%r\ %w\ %y\ \ <%{&fileencoding},%{&fileformat}>,\ \ \ \ \ \ %l,%c%V\ of\ %L\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ %P

" Insert date
:map \dd :r! date +\%m/\%d/\%y; <CR> ; <ESC> k J

" For python 
:map \pp :! time nohup nice -10 python3  % > %:r.py_out <CR> :e %:r.py_out <CR> 



" For r
:map \rr :! nohup nice -10 R CMD BATCH --vanilla % %:r.out <CR> :e %:r.out <CR> 
:map \rh :! nice -10 Rscript -e "library(knitr); knit('%')" <CR> :!mv %:t:r.md %:r.md <CR> :e %:r.md <CR>

" For stata
:map \ss :! stata  < % > %:r.log <CR> :e %:r.log <CR>

set swapfile
set dir=~/.vim/tmp

set t_Co=256

" Copilot enable
let g:copilot#enable_at_startup = 1

colors bluegreen


" Get r filetype 
au BufRead,BufNewFile * if &ft == 'rexx' | set ft=r | endif

" Plugins
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'lervag/vimtex'
Plugin 'quarto-dev/quarto-vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
