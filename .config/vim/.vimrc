" Vim config | version 8 via homebrew 
" 
" Maintainer:         Kyle Gortych 
" Date last modified: 06-08-2022

" Plugins 
call plug#begin('~/.vim/plugged')
Plug 'widatama/vim-phoenix'
" Plug 'dylanaraps/wal.vim'
Plug 'sjl/vitality.vim'
Plug 'dense-analysis/ale'
Plug 'sheerun/vim-polyglot'
Plug 'valloric/youcompleteme'
Plug 'tmsvg/pear-tree'
Plug 'romainl/vim-cool'
Plug 'tibabit/vim-templates'
Plug 'tounaishouta/coq.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/powerline-extra-symbols'
Plug 'ryanoasis/vim-devicons'
call plug#end()

" airline 
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_left_sep = "\uE0BC"
let g:airline_right_sep = "\uE0BE"

" let g:airline#extensions#tabline#left_sep = '\uE0BC'
" let g:airline#extensions#tabline#left_alt_sep = '\uE0BC'
" let g:airline#extensions#tabline#right_sep = '\uE0BE'
" let g:airline#extensions#tabline#right_alt_sep = '\uE0BE'

let g:airline_theme='lucius'

" devicons 
" Out side vim env apply icon to nnn file manager, starship prompt

" let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
" let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.*jquery.*\.js$'] = 'ƛ'


let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.vimrc'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['vimrc'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.gvimrc'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['_gvimrc'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['test.vim'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.vim'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['vim'] = ''

" overide color scheme 
"autocmd ColorScheme * highlight Normal ctermfg=black ctermbg=208
autocmd ColorScheme * highlight NonText ctermfg=208 ctermbg=NONE
" autocmd ColorScheme * highlight ColorColumn ctermfg=208 ctermbg=black
autocmd ColorScheme * highlight Folded ctermfg=black ctermbg=245
" autocmd ColorScheme * highlight FoldColumn ctermfg=white ctermbg=white
autocmd ColorScheme * highlight Search ctermfg=black ctermbg=208
autocmd ColorScheme * highlight ModeMsg ctermfg=208 ctermbg=NONE
autocmd ColorScheme * highlight MoreMsg ctermfg=208 ctermbg=NONE
autocmd ColorScheme * highlight WarningMsg ctermfg=black ctermbg=208
autocmd ColorScheme * highlight SpellBad term=bold,underline cterm=bold,underline ctermfg=9 ctermbg=8 guifg=#EFEFEF guibg=#515151
autocmd ColorScheme * highlight SpellRare cterm=bold,underline ctermfg=220 ctermbg=8
autocmd ColorScheme * highlight SpellLocal cterm=bold,underline ctermfg=208 ctermbg=8
autocmd ColorScheme * highlight airline_tabmod ctermfg=black ctermbg=208
autocmd ColorScheme * highlight Visual ctermfg=black ctermbg=208
autocmd ColorScheme * highlight DiffAdd ctermfg=black ctermbg=39
" autocmd ColorScheme * highlight airline_Tabtype ctermfg=black ctermbg=39

" terminal cursor shape overide 
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=2\x7" " Underscore in insert mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
    autocmd VimLeave * let &t_me = "\<Esc>]50;CursorShape=2\x7"
endif

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

" spelling 
set spell spelllang=en_us
set spellfile=en.utf-8.add
set complete+=kspell
set secure

" wildmenu 
set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx"

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

" \    'html': ['tidy-html5'],
" \    'htm': ['tidy-html5'],
" \    'css': ['csslint']
" 
" each language has linter, fixer, and compiler
" try to orginize by one package manager

" compiled languages require a makefile
"let g:ale_language-name_parse_makefile = 1

"if compile to json such as node or cargo prj struct
"let g:ale_parse_compile_commands = 1 

" check mactex for preinstalled linter and fixer and compiler
" should have packages like zed-csp aka z notation and tikz-uml

"js and jsx use eslint also set up airbnb in json 

" \    'html': [''],
" \    'css': [''],
" \    'javascript': [''],
" \    'jsx': [''],
" \    'java': [''],

" ycm 


" vim template 
" edit templates in ~/.vim/plugged/vim-templates/templates
" WARNING PlugUpdate could cause loss of template configs store on git or
" seprate txt file

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

" save folds 
augroup remember_folds
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
augroup END

" Lex Netrw 
let g:netrw_winsize=20
" each tab | separate prj folder
" minimizes buffer list | two per tab

" defaults 
colorscheme phoenix 
syntax on
filetype on
filetype plugin on
filetype indent on
set clipboard=unnamed
set nocp
set number
set tabstop=2
set shiftwidth=2
set expandtab
set backspace=2
set autoindent
set encoding=UTF-8
set tw=45
set nowrap 
set fo-=t fo-=c
" set wrap linebreak nolist
" set whichwrap+=<,>,h,l
" set cc=78
set cursorline
set hidden
set scrolloff=5
set foldopen-=block
set foldopen-=hor

" highlighting word searches with / 
set incsearch
set hlsearch

" visual block increment 
set nrformats=bin,alpha,octal,hex
