" vundle setup
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" other plugins
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'esneider/YUNOcommit.vim'
Plugin 'plasticboy/vim-markdown'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/ctrlp.vim'
Plugin 'vim-erlang/vim-erlang-compiler'
Plugin 'vim-erlang/vim-erlang-runtime'
Plugin 'vim-erlang/vim-erlang-omnicomplete'
Plugin 'vim-erlang/vim-erlang-tags'
Plugin 'vim-erlang/vim-erlang-skeletons'
Plugin 'nathanaelkane/vim-indent-guides'

call vundle#end()
filetype plugin indent on
" end vundle setup

set backspace=indent,eol,start

set encoding=utf-8  " The encoding displayed.¬
set fileencoding=utf-8  " The encoding written to file.¬
set t_Co=256

"cycle line numbers/relative line numbers/no line numbers. default: relative
function! NumberToggle()
	if(&relativenumber == 1)
		set number
		set norelativenumber
	elseif(&relativenumber == 0 && &number == 1)
		set nonumber
	else
		set relativenumber
		set number
	endif
endfunc
map <silent><F10> :call NumberToggle()<CR>
set number
set relativenumber

" toggle unprintable characters visible
map <silent><F11> :set invlist<CR>

" toggle between terminal and vim mouse, default vim mouse
function! ToggleMouseMode()
    let &mouse=(&mouse == "a"?"":"a")
endfunction
map <silent><F12> :call ToggleMouseMode()<CR>
imap <silent><F12> <Esc> :call ToggleMouseMode()<CR>
set mouse=a

" tab indentation
set noexpandtab
set copyindent
set softtabstop=0
set shiftwidth=4
set tabstop=4
set autoindent

set wildmenu

set incsearch
set hlsearch
set ignorecase
set smartcase

set laststatus=2
set showtabline=2
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
map <silent><F8> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" vim-airline
let g:airline_theme='solarized'
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
set statusline+=%#warningmsg#
set statusline+=%*

" gitgutter
let g:gitgutter_enabled = 1
map <silent><F9> :GitGutterToggle<CR>

" vim-indent-guides
autocmd VimEnter * :IndentGuidesEnable


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

" auto reload .vimrc
" credits: sirlancelot http://stackoverflow.com/questions/2400264/is-it-possible-to-apply-vim-configurations-without-restarting/2403926#2403926 
augroup myvimrc
    au!
    au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
augroup END

" quit with error code
" useful when git throws continuous diffs to vim when used as difftool
if &diff
	map Q :cquit<CR>
  endif
