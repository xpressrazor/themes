set nocompatible
set hidden
syntax on          " syntax highlighing
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase, case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to shiftwidth, not tabstop
set incsearch     " show search matches as you type

set guifont=Inconsolata\ Medium\ 15 
" set guioptions-=m	"menubar
set guioptions-=T	" toolbar
set guioptions-=r	" right scrollbar

set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell           " don't beep
set noerrorbells         " don't beep
filetype plugin indent on

" Mark
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

" Fold
" v(visual) zf = create fold, za = open, zc = close, zd = delete fold
augroup vimrc
  au BufReadPre * setlocal foldmethod=indent
  au BufWinEnter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
augroup END

" Use "ctags -R ~/src/dir" to create tags file
" set tags=./tags,tags,~/.tags/tags,../tags,../../tags;$HOME
set tags=./tags,~/.tags/tags,../tags,../../tags

" Key Mapping
" ===========
"
set pastetoggle=<F2>
imap <F3> <C-o>:setlocal spell! spelllang=en_us<CR>
nmap <F3> :setlocal spell! spelllang=en_us<CR>
map <F4> :NERDTreeToggle<CR>

" mouse
set mouse=a
nnoremap ; :

" For wrappend lines
nnoremap j gj
nnoremap k gk

" Templates
" autocmd BufNewFile  *.c	0r ~/.vim/template/skeleton.c
" autocmd BufNewFile  *.cpp	0r ~/.vim/template/skeleton.cpp
" autocmd BufNewFile  *.h	0r ~/.vim/template/skeleton.h
" autocmd BufNewFile  *.java	0r ~./vim/template/skeleton.java


" set path=$PWD/**

" Compiler
set makeprg=[[\ -f\ Makefile\ ]]\ &&\ make\ \\\|\\\|\ gcc\ %\ -o\ %<
" CDC = Change to Directory of Current file
" command CDC cd %:p:h
command MAIN r ~/.vim/template/skeleton.c


set autochdir
nnoremap gb :buffers<CR>:

" Compile and run

nmap <C-c> :!gcc % -o /tmp/%< && clear && /tmp/%< <CR>
inoremap <C-d> <ESC>O
imap <C-Space> <ESC>
imap <C-k> <ESC>:bn<CR>
imap <C-l> <ESC>:bp<CR>
nmap <C-k> :bn<CR>
nmap <C-l> :bp<CR>
imap jj <ESC>


au FileType c,cpp setlocal comments-=:// comments+=f://

" au VimEnter * !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
