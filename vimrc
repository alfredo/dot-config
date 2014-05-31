" vi < vim
set nocompatible
syntax on
let python_highlight_all = 1
set viminfo='1000,f1,:1000,/1000
set history=500

" use ack instead of grep
set grepprg=ack\ -H

" Automatically reload .vimrc when changing
autocmd! bufwritepost .vimrc source %
set autoread

" Use filetype plugins
filetype plugin on
filetype on

" Store temporary files in a central spot
set backupdir=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp

"=====
" file presentation
"=====
filetype indent on
set autoindent
set smartindent
set backspace=indent,eol,start
set textwidth=80
set nowrap
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4
" don't force # to column 0
inoremap # X<BS>#

" use two space tabs for ruby
autocmd FileType ruby setlocal shiftwidth=2 softtabstop=2 et

" function to turn on ruby tab setup
function! TwoTabs()
    set shiftwidth=2
    set softtabstop=2
endfunction
nmap <F9> mz:execute TwoTabs()<CR>'z

" display whitespace at start and end of lines
set list
set listchars=tab:>-,trail:-

"=====
" vim presentation
"=====
" Show line numbers by default
set number

" 256 colours
set t_Co=256

" just in case the host termcap on this machine sucks, give me color.
if &term =~ "xterm" || &term =~ "screen"
    set t_Co=256
    if has("terminfo")
        set t_Sf=[3%p1%dm
        set t_Sb=[4%p1%dm
    else
        set t_Sf=[3%dm
        set t_Sb=[4%dm
    endif
endif

" Show nice info in ruler
set ruler
set laststatus=2

" show encoding in statusline if available
if has("statusline")
 set statusline=%<%f\ %h%m%r%=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P
endif

" Enable folding
set foldenable
" set foldmethod=indent

" Autoclose folds, when moving out of them
set foldclose=all

" Use incremental searching
set incsearch
set ignorecase
set smartcase

" Do highlight search results
set hlsearch

" Jump 5 lines when running out of the screen
set scrolljump=5

" Indicate jump out of the screen when 3 lines before end of the screen
set scrolloff=3

" tab complete menu
set wildmenu
set wildmode=longest,full

" set behaviour for onmicomplete menu
set completeopt=longest,menuone

if has("gui_running")
    if has("macunix")
        set guifont=Andale\ Mono:h12
    else
        set gfn=Inconsolata\ 10
    endif
    set guioptions-=T
    set guioptions-=e
    set guioptions-=r
    set guioptions-=L
endif

"=====
" Mappings
"=====

" unhighlight search results
map <silent> <C-N> :noh<CR>

" save current file as root
cmap w!! w !sudo tee %

" make :W the same as :w
cmap W w

" Toggle file browser and tag list
nmap <silent> nt :NERDTreeToggle<CR>
nmap <silent> tl :TlistToggle<CR><C-w>h

" Don't move the curosr when highlighting
nmap * *N
nmap # #N

" clean whitespace only lines
map <silent> <leader>cel mz:%s:\s\+$::<CR>'z:delmarks z<CR>

" execute current ruby file
map <C-F11> <Esc>:!ruby %<CR>

" search for the word under the cursor in all files
" in this directory and below
map <F4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR>

" toggle showmarks
map <silent> <F7> <ESC>:ShowMarksToggle<CR>

" load project
map np :Project $HOME/.vim/projects/
map <silent><F2> <Plug>ToggleProject

" tab bindings
map <C-t> <Esc>:tabe<CR>
map gb <Esc>:tabprevious<CR>

" easier omnicomplete binding
inoremap <Nul> <C-x><C-o>

" leave insert mode
imap ;; <Esc>

" shortcut to paste from clipboard
map <C-p> "+gp

"=====
" plugin settings
"=====
"let g:EnhCommentifyUseAltKeys    = 'yes'
let g:EnhCommentifyPretty        = 'yes'
let g:EnhCommentifyRespectIndent = 'yes'
let g:EnhCommentifyUseSyntax     = 'yes'
let g:EnhCommentifyUseBlockIndent = 'yes'
let g:EnhCommentifyMultiPartBlocks = 'yes'
let g:EnhCommentifyAlignRight = 'yes'

" showmarks settings
let g:showmarks_enable=0
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let g:showmarks_textlower="\t"
let g:showmarks_textupper="\t"
let g:showmarks_textother="\t"

" snipmate settings
" autocmd FileType python set ft=python.django
autocmd FileType html set ft=html.django_template

" taglist
let Tlist_Exist_OnlyWindow = 1
let Tlist_Compact_Format = 1
let Tlist_Close_On_Select = 1
let Tlist_Enable_Fold_Column = 0

" set the supertab complete type
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"


" fuzzyfinder
map <leader>t :FufFile<CR>

"=====
" highlighting
"=====
" mark line chars in excess of 80
hi LineTooLong cterm=bold ctermfg=LightYellow ctermbg=Black guibg=Black
match LineTooLong /\%81v.\+/

" set highlight colours for the omnicomplete menu
hi Pmenu ctermfg=0 ctermbg=DarkGreen guifg=#ffffff guibg=#cb2f27
hi PmenuSel ctermfg=0 ctermbg=LightGreen guifg=#ffffff guibg=#ff3333
hi PmenuSbar ctermfg=9 ctermbg=0 guifg=#ffffff guibg=#cb2f27
hi PmenuThumb ctermfg=0 ctermbg=7 guifg=#ffffff guibg=#cb2f27

" colours for the showmarks plugin
" hi ShowMarksHLl cterm=bold ctermbg=none
hi ShowMarksHLl cterm=bold ctermbg=none ctermfg=blue guifg=#038bbe guibg=NONE
hi ShowMarksHLu cterm=bold ctermbg=none ctermfg=red
hi ShowMarksHLo cterm=bold ctermbg=none ctermfg=yellow
hi ShowMarksHLm cterm=bold ctermbg=none ctermfg=green
hi SignColumn   cterm=bold ctermbg=none guibg=NONE
" hi LineNr guibg=NONE

" taglist
hi MyTagListFileName guibg=NONE guifg=green

" pyflakes
hi SpellBad guibg=#c2473b

" copy to the xclipboard
set clipboard+=unnamed


"=====
" abbreviations
"=====
ab bl <Esc>bi{% block <Esc>ea %}{% endblock %}<Esc>h%i


" colour
" https://github.com/altercation/vim-colors-solarized
set background=dark
" git
autocmd Filetype gitcommit setlocal spell textwidth=72
