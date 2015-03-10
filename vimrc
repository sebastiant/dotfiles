set nocompatible

set t_Co=256

" pathogen
execute pathogen#infect()

" toggle line numbers, default on
map <silent><F10> :set invnumber<CR>
set number

" toggle unprintable characters visible
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
set background=dark
colorscheme solarized
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"

" NERDTree
map <silent><F9> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" vim-airline
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" syntastic
let g:airline#extensions#syntastic#enabled = 1
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>
