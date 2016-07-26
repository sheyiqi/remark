set nocompatible               " be iMproved

"=============================== vundle ================================
filetype off                   " required!
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"" let Vundle manage Vundle, Required!
Plugin 'VundleVim/Vundle.vim'

"" from github.com/xxx/yyy.git
Plugin 'Lokaltog/vim-easymotion'
Plugin 'ctrlp.vim'
"Plugin 'nerdcommenter'
"Plugin 'fencview'

"" from github.com/vim-scripts/yyy.git
Plugin 'ShowTrailingWhitespace'
Plugin 'Align'
"Plugin 'bufexplorer.zip'
"Plugin 'grep.vim'

"" repos init by adamzhang
Plugin 'vimcdoc'
Plugin 'yaml.vim'
"Plugin 'file:///home/mcrd/engineer/cadman/repos/vim/vim-jinja'

"
" Brief help
" :PluginList          - list configured bundles
" :PluginInstall(!)    - install(update) bundles
" :PluginSearch(!) foo - search(or refresh cache first) for foo
" :PluginClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Plugin command are not allowed..
"------------------------------- vundle --------------------------------

call vundle#end()

" Enable filetype plugins
filetype plugin on
" Enable filetype indent
filetype indent on

if has("win32unix")
    colo desert
endif

if &t_Co > 1 || has("gui_running")
    syntax enable
    set hlsearch
endif
"" more subtle popup colors
if has('gui_running')
    " set guifont=DejaVu\ Sans\ Mono\ 12
    set guifont=Monaco\ 10
    set cursorline
    highlight CursorLine guibg=#99ffee
    highlight Pmenu guibg=#cccccc gui=bold
else
    colo default
endif

let mapleader = ","
let g:mapleader = ","

" Set to auto read when a file is changed from the outside
set autoread
set backspace=indent,eol,start
set history=1024
set showcmd
set visualbell
set noincsearch
set hlsearch
set number
set ruler

set tabstop=4
set shiftwidth=4
set expandtab

"autocmd filetype html     set shiftwidth=2
"autocmd filetype html     set tabstop=2
"autocmd filetype yaml     set shiftwidth=2
"autocmd filetype yaml     set tabstop=2
"autocmd filetype xml     set shiftwidth=2
"autocmd filetype xml     set tabstop=2

autocmd BufNewFile,BufRead spec.conf set filetype=yaml
autocmd BufNewFile,BufRead *.ds      set filetype=rst
autocmd BufNewFile,BufRead *.sage    set filetype=python

"if v:version >= 703
"    set undofile
"    set undolevels=1024
"endif
"
map gf :tabedit <cfile><CR>



"Tab configuration
nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>te :tabedit
nnoremap <leader>tc :tabclose<CR>
nnoremap <leader>tm :tabmove
nnoremap <leader>e :tabnew <C-r>=expand("%:p:h") . "/"<CR>

"Add line-scroll in edit-mode
inoremap <C-e> <C-x><C-e>
inoremap <C-y> <C-x><C-y>

nnoremap <C-j> <C-e>j
nnoremap <leader>vs :vertical split
nnoremap <leader>l :set list!<CR>
set listchars=tab:»\ ,eol:↓

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return '[P]'
    en
    return ' '
endfunction
set laststatus=2
set statusline=%y\ %-4.{HasPaste()}\ %r\ %{getcwd()}%h/%f\ %m\ \ %p%%\ L%c:C%l

function RemoveTrailingWhitespace()
    if &ft != "diff"
        let b:curcol = col(".")
        let b:curline = line(".")
        silent! %s/\s\+$//
        silent! %s/\(\s*\n\)\+\%$//
        call cursor(b:curline, b:curcol)
    endif
endfunction

function! LookupInSdcv()
    let liscword = expand("<cword>")
    if liscword !~ '^[a-zA-Z]\+$'
        let liscword = input("Enter word or phrase: ")
    endif
    let liscword = "\'" . liscword . "\'"
    new
    set buftype=nofile nonumber
    exe "%!sdcv " . liscword
endfunction

map <leader>q :call LookupInSdcv()<CR>

"============================= YouCompleteMe ================================
"let g:ycm_global_ycm_extra_conf='~/.ycm_extra_conf.py'
let g:ycm_autoclose_preview_window_after_completion=1
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
"----------------------------- YouCompleteMe --------------------------------

" highlight rightMargin term=bold ctermfg=brown guifg=brown
" match rightMargin /.\%>82v/

" Remove the Windows ^M - when the encodings gets messed up
noremap <leader>sm mmHmt:%s/<C-v><CR>//ge<CR>'tzt'm

"============================= CtrlP ================================
" Default key is <C-p> but is eat by YankRing, so ,pp instead
nnoremap <leader>pp :CtrlP<CR>
let g:ctrlp_custom_ignore='\v[\/]\.(git|hg|svn)$'
let g:ctrlp_max_height=15
let g:ctrlp_follow_symlink=1
"----------------------------- CtrlP --------------------------------

" XXX
"vnoremap <leader>cc :NERDComToggleComment


"============================= YankRing ================================
let g:yankring_history_dir = '$HOME/tmp'

