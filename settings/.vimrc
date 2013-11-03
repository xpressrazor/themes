set nocompatible
set hidden
syntax on          " syntax highlighing


"set nowrap        " don't wrap lines
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
"set hlsearch      " highlight search terms
set incsearch     " show search matches as you type

set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
"set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep

"set nobackup
"set noswapfile

set pastetoggle=<F2>
imap <F3> <C-o>:setlocal spell! spelllang=en_us<CR>
nmap <F3> :setlocal spell! spelllang=en_us<CR>

" mouse
set mouse=a
nnoremap ; :

" format
vmap Q gq
nmap Q gqap

" For wrappend lines
nnoremap j gj
nnoremap k gk

"nmap <silent> ,/ :nohlsearch<CR>
"cmap w!! w !sudo tee % >/dev/null

map <F4> :wa<cr>:make<cr>
imap xx <Esc>
"map f :bn<cr>
"map b :bp<cr>

" repeat previous command
map <S-k> <Esc>:@:<CR>

" Copy paste
"nnoremap <C-y> "+y
"vnoremap <C-y> "+y
"nnoremap <C-p> "+gP
"vnoremap <C-p> "+gP

"set tags=~/.tags;
set tags=~/.tags/java.tags
set complete=.,w,b,u,t,i
