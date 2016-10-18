runtime bundle/vim-pathogen/autoload/pathogen.vim

execute pathogen#infect()

colo desert
syntax enable
filetype plugin indent on

let g:netrw_liststyle=3

let g:org_agenda_files = ['~/Dropbox/org/*.org']

nnoremap <leader>. :CtrlPTag<cr>
set tags=./.tags,.tags,./tags,tags
let g:scala_sort_across_groups=1
