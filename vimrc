runtime bundle/vim-pathogen/autoload/pathogen.vim

execute pathogen#infect()

colo desert
syntax enable
filetype plugin indent on

let g:netrw_liststyle=3

let g:org_agenda_files = ['~/Dropbox/org/*.org']

nnoremap <leader>. :CtrlPTag<cr>
let maplocalleader=","
set tags=./.tags,.tags,./tags,tags
let g:scala_sort_across_groups=1

silent! nmap <C-p> :NERDTreeToggle<CR>
silent! map <F2> :NERDTreeToggle<CR>
silent! map <F3> :NERDTreeFind<CR>
let g:NERDTreeToggle="<F2>"
let g:NERDTreeMapActivateNode="<F3>"
let g:NERDTreeMapPreview="<F4>"

autocmd filetype crontab setlocal nobackup nowritebackup

"############
"# QuickSBT #
"############

"# Launch SBT with support for generating /tmp/sbt.quickfix file for Vim
"# http://github.com/aloiscochard / https://gist.github.com/4698501

"# Error format for SBT, and shortcut to open SBT quickfix file :
"# -----vim.rc-------
set errorformat=%E\ %#[error]\ %#%f:%l:\ %m,%-Z\ %#[error]\ %p^,%-C\ %#[error]\ %m
set errorformat+=,%W\ %#[warn]\ %#%f:%l:\ %m,%-Z\ %#[warn]\ %p^,%-C\ %#[warn]\ %m
set errorformat+=,%-G%.%#
noremap <silent> <Leader>ff :cf target/sbt.quickfix<CR>
noremap <silent> <Leader>fn :cn<CR>

