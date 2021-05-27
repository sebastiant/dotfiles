{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-colors-solarized
      vim-airline
      vim-airline-themes
      nerdtree
      vim-markdown
      ctrlp
      vim-indent-guides
    ];
    extraConfig = ''
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
      " enable transparency for alacritty
      hi Normal ctermbg=NONE guibg=NONE
  
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
  
      " vim-indent-guides
      autocmd VimEnter * :IndentGuidesEnable
  
  
      " disable old habits
      map <Left> <Nop>
      map <Right> <Nop>
      map <Up> <Nop>
      map <Down> <Nop>
  
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
  
      " CtrlP to ignore .gitignore specs
      " credits: https://medium.com/a-tiny-piece-of-vim/making-ctrlp-vim-load-100x-faster-7a722fae7df6
      let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
  
      " Fugitive
      command! Greview :Git! diff --staged
      nnoremap <leader>gr :Greview<cr>
    '';
  };
}
