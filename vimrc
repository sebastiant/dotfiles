set nocompatible

" vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" vundle plugins
Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'Yggdroot/indentLine'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'esneider/YUNOcommit.vim'
Plugin 'edkolev/erlang-motions.vim'
Plugin 'jimenezrick/vimerl'
Plugin 'lambdatoast/elm.vim'

call vundle#end()
filetype plugin indent on
" end vundle setup

set t_Co=256

set encoding=utf-8  " The encoding displayed.¬
set fileencoding=utf-8  " The encoding written to file.¬

" toggle line numbers, default on
map <silent><F10> :set invnumber<CR>
set number

" toggle unprintable characters visible
map <silent><F11> :set invlist<CR>

" toggle between terminal and vim mouse, default vim mouse
map <silent><F12> :call ToggleMouseMode()<CR>
imap <silent><F12> :call ToggleMouseMode()<CR>
function ToggleMouseMode()
    let &mouse=(&mouse == "a"?"":"a")
endfunction
set mouse=a

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
set list
set listchars=tab:▸\ ,eol:¬
syntax enable

filetype indent on

" solarized colors with settings
set background=dark
colorscheme solarized
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"

" Plugin configuration
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

" indentLine
let g:indentLine_char = '︙'

" airline
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" syntastic
" Show location view if syntax errors exist
let g:syntastic_auto_loc_list = 1
" Jump to first syntax error on save or open
let g:syntastic_auto_jump = 2
let g:airline#extensions#syntastic#enabled = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_erlang_checkers=['syntaxerl']


" disable old habits
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

" hjkl for window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" , for custom commands
let mapleader = ","
" clear search
nnoremap <leader><space> :noh<cr>


