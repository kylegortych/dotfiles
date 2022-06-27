"///////////////////////////////////////////////////////////
"         _                              _      
"  _   __(_)___ ___  __________   _   __(_)___ _
" | | / / / __ `__ \/ ___/ ___/  | | / / / __ `/
" | |/ / / / / / / / /  / /__    | |/ / / /_/ / 
" |___/_/_/ /_/ /_/_/   \___/    |___/_/\__,_/  
"     __                         __                     
"    / /_  ____  ____ ___  ___  / /_  ________ _      __
"   / __ \/ __ \/ __ `__ \/ _ \/ __ \/ ___/ _ \ | /| / /
"  / / / / /_/ / / / / / /  __/ /_/ / /  /  __/ |/ |/ / 
" /_/ /_/\____/_/ /_/ /_/\___/_.___/_/   \___/|__/|__/  
" 
" Maintainer:         Kyle Gortych 
" Date last modified: 06-27-2022
"///////////////////////////////////////////////////

" Plugins 
runtime */jetpack.vim
call jetpack#begin()
Jetpack 'widatama/vim-phoenix'
" Jetpack 'dylanaraps/wal.vim'
Jetpack 'sjl/vitality.vim'
Jetpack 'dense-analysis/ale'
Jetpack 'valloric/youcompleteme'
Jetpack 'tmsvg/pear-tree'
Jetpack 'romainl/vim-cool'
Jetpack 'tibabit/vim-templates'
Jetpack 'tounaishouta/coq.vim'
call jetpack#end()

" overide color scheme 
"autocmd ColorScheme * highlight Normal ctermfg=black ctermbg=208
autocmd ColorScheme * highlight NonText ctermfg=208 ctermbg=NONE
" autocmd ColorScheme * highlight ColorColumn ctermfg=208 ctermbg=black
autocmd ColorScheme * highlight Folded ctermfg=black ctermbg=245
" autocmd ColorScheme * highlight FoldColumn ctermfg=white ctermbg=white
autocmd ColorScheme * highlight Search ctermfg=black ctermbg=208
autocmd ColorScheme * highlight ModeMsg ctermfg=208 ctermbg=NONE
autocmd ColorScheme * highlight MoreMsg ctermfg=208 ctermbg=NONE
autocmd ColorScheme * highlight WarningMsg ctermfg=NONE ctermbg=208
autocmd ColorScheme * highlight Todo ctermfg=208 ctermbg=NONE
autocmd ColorScheme * highlight SpellCap cterm=bold ctermfg=black ctermbg=208
autocmd ColorScheme * highlight SpellBad cterm=bold ctermfg=9 ctermbg=8 guifg=#EFEFEF guibg=#515151
autocmd ColorScheme * highlight SpellRare cterm=bold ctermfg=220 ctermbg=8
autocmd ColorScheme * highlight SpellLocal cterm=bold ctermfg=208 ctermbg=8
"autocmd ColorScheme * highlight airline_tabmod ctermfg=black ctermbg=208
autocmd ColorScheme * highlight Visual ctermfg=black ctermbg=208
autocmd ColorScheme * highlight DiffAdd ctermfg=black ctermbg=39
autocmd ColorScheme * highlight DiffChange ctermfg=black ctermbg=245
autocmd ColorScheme * highlight DiffDelete ctermfg=black ctermbg=245

" pear tree 
let g:pear_tree_pairs ={
\ '(': {'closer': ')'},
\ '[': {'closer': ']'},
\ '{': {'closer': '}'},
\ "'": {'closer': "'"},
\ '"': {'closer': '"'},
\ '<': {'closer': '>'},
\ '<!-': {'closer': '- -->'},
\ '<!D': {'closer': 'OCTYPE html>'},
\ '<ht': {'closer': 'ml></html>'},
\ '<he': {'closer': 'ad></head>'},
\ '<header': {'closer': '></header>'},
\ '<ti': {'closer': 'tle></title>'},
\ '<sc': {'closer': 'ript></script>'},
\ '<st': {'closer': 'yle></style>'},
\ '<bo': {'closer': 'dy></body>'},
\ '<ta': {'closer': 'ble></table>'},
\ '<tr': {'closer': '></tr>'},
\ '<td': {'closer': '></td>'},
\ '<di': {'closer': 'v></div>'},
\ '<ul': {'closer': '></ul>'},
\ '<ol': {'closer': '></ol>'},
\ '<li': {'closer': '><a></a></li>'},
\ '<na': {'closer': 'v></nav>'},
\ '<pa': {'closer': 'th></path>'},
\ '<bu': {'closer': 'tton></button>'},
\ '<sv': {'closer': 'g></svg>'},
\ '<a': {'closer': '></a>'},
\ '<ma': {'closer': 'in></main>'},
\ '<h1': {'closer': '></h1>'},
\ '<h2': {'closer': '></h2>'},
\ '<h3': {'closer': '></h3>'},
\ '<se': {'closer': 'ction></section>'},
\ '<p': {'closer': '></p>'}
\}

" ale 
set omnifunc=syntaxcomplete#Complete
let g:ale_lint_on_enter = 0
let g:ale_completion_enabled = 1
let g:rustfmt_autosave = 1
" let g:ale_linters_explicit = 1
" set omnifunc=ale#completion#OmniFunc

let g:ale_linters = {
\    'bash': ['shellcheck'],
\    'rust': ['cargo', 'rustfmt'],
\    'python': ['pylint'],
\    'java': ['uncrustify'],
\    'javascript': ['eslint']
\}
 
let g:ale_fixers = {
\    'bash': ['shellcheck'],
\    'rust': ['rustfmt'],
\    'python': ['autopep8'],
\    'java': ['uncrustify'],
\    'javascript': ['eslint']
\}

" ycm 


" vim template 
" edit templates in ~/.vim/plugged/vim-templates/templates
" WARNING PlugUpdate could cause loss of template configs store on git or
" seprate txt file

" defaults 

" main defaults
colorscheme phoenix 
syntax on
filetype on
filetype plugin on
filetype indent on
set clipboard=unnamed
\ nocp
\ number
\ tabstop=2
\ shiftwidth=2
\ expandtab
\ backspace=2
\ autoindent
\ encoding=UTF-8
\ lz
\ tf 
\ smc=80
\ tw=45
\ nowrap 
\ fo-=t 
\ fo-=c
" set wrap linebreak nolist
" set whichwrap+=<,>,h,l
" set cc=78
\ cul
\ hidden
\ scrolloff=5
\ foldopen-=block
\ foldopen-=hor
\ incsearch
\ hlsearch
\ nrformats=bin,alpha,octal,hex
" spelling 
\ spell spelllang=en_us
\ spellfile=en.utf-8.add
\ complete+=kspell
\ secure
" wildmenu 
\ wildmenu
\ wildmode=list:longest
\ wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx"
" Lex Netrw 
let g:netrw_winsize=20
" each tab | separate prj folder
" minimizes buffer list | two per tab

" remappings current [0] 
" remappinig for spell and suyntax error jumping same as / command's jumping n

" noremap n ]s

" remapping for para jump curly braces and horizontal word jump e and b  

"noremap hjkl | hold 3 jumps | (h -> b) & (l -> e) & (j -> }) & (k -> {)

" macros 

" invoke single line via @letter

" to clear use :let @letter = ''

" multi line: shift v command 
" result: :'<,'>normal @letter 

" Outline
" @letter:      comands
" macro result: result

" @a: 
" macro result:

" functions

" terminal cursor shape overide 
if $TERM_PROGRAM =~ "iTerm"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
    let &t_SI = "\<Esc>]50;CursorShape=2\x7" " Underscore in insert mode
    autocmd VimLeave * let &t_me = "\<Esc>]50;CursorShape=2\x7"
endif

" save folds 
" conflict | :diffoff! can't override
" workaround | manual reset :mkview :loadview 
augroup remember_folds
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
augroup END

"statusline git 


" statusline 
set ls=2
\ stl+=%#Search#
\ stl+=%f\ %m\ %y\ %{&fileencoding?&fileencoding:&encoding}\ [%{&fileformat}\]
\ stl+=%#CursorLineFold#
\ stl+=%=%#Search#
\ stl+=\ Buff:\%n\ Pos:\%l\:\%c 
