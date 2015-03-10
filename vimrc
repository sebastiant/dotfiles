" toggle line numbers, default on
map <silent><F10> :set invnumber<CR>
set number

" toggle unrprintable characters visible
map <silent><F11> :set invlist<CR>

" toggle between terminal and vim mouse
map <silent><F12> :let &mouse=(&mouse == "a"?"":"a")<CR>:call ShowMouseMode()<CR>
imap <silent><F12> :let &mouse=(&mouse == "a"?"":"a")<CR>:call ShowMouseMode()<CR>
function ShowMouseMode()
    if (&mouse == 'a')
        echo "mouse-vim"
    else
        echo "mouse-xterm"
    endif
endfunction

" 4 spaced tabs 
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

set wildmenu

set incsearch
set hlsearch
set ignorecase
set smartcase

set laststatus=2
set showtabline=2
set autoindent
syntax enable

filetype plugin on
filetype indent on

" solarized colors with settings 
colorscheme solarized
set background=dark
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"

" pathogen
execute pathogen#infect()

" NERDTree
map <silent><F9> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>
