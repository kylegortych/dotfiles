# vimrc 
<details>
  <summary>heading</summary>
  
  ```vim
  " Vim config | version 8 via homebrew 
  " 
  " Maintainer:         Kyle Gortych 
  " Date last modified: 04-17-2022
  ```
</details>

<details>
  <summary>plugins</summary>
  
  ```vim
  " Plugins 
  call plug#begin('~/.vim/plugged')
  Plug 'mhinz/vim-startify'
  Plug 'nikolvs/vim-sunbather'
  Plug 'fcpg/vim-fahrenheit'
  Plug 'bruth/vim-newsprint-theme'
  Plug 'sjl/vitality.vim'
  Plug 'dense-analysis/ale'
  Plug 'sheerun/vim-polyglot'
  Plug 'valloric/youcompleteme'
  Plug 'tmsvg/pear-tree'
  Plug 'romainl/vim-cool'
  Plug 'tibabit/vim-templates'
  Plug 'tounaishouta/coq.vim'
  " Plug 'szw/vim-ctrlspace'
  Plug 'yuttie/comfortable-motion.vim'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'ryanoasis/powerline-extra-symbols'
  Plug 'ryanoasis/vim-devicons'
  call plug#end()
  ```
</details>

<details>
  <summary>startify</summary>
  
  ```vim
  " startify 
  let g:startify_custom_header = [
      \ " ",
      \ ' _    ___         ',
      \ '| |  / (_)___ ___ ',
      \ '| | / / / __ `__ \',
      \ '| |/ / / / / / / /',
      \ '|___/_/_/ /_/ /_/ ',
      \ ' ',
      \ ]

  " File lim
  let g:startify_files_number = 5

  " Update session auto on exit 
  let g:startify_session_persistence = 1

  " List files and sessions
  let g:startify_lists = [
    \ { 'type': 'dir',       'header': ['   Recent files'] },
    \ { 'type': 'sessions',  'header': ['   Saved sessions'] },
    \ ]
  ```
</details>

<details>
  <summary>airline</summary>
  
  ```vim
  " airline 
  let g:airline_powerline_fonts = 1

  if !exists('g:airline_symbols')
      let g:airline_symbols = {}
  endif

  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#tabline#formatter = 'default'
  let g:airline#extensions#whitespace#enabled = 0
  let g:airline_left_sep = "\uE0BC"
  let g:airline_right_sep = "\uE0BE"

  let g:airline_theme='lucius'
  ```
</details>

<details>
  <summary>devicons</summary>
  
  ```vim
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
  ```
</details>

<details>
  <summary>comfortable motion</summary>
  
  ```vim
  " comfortable motion 
  let g:comfortable_motion_scroll_down_key = "j"
  let g:comfortable_motion_scroll_up_key = "k"
  ```
</details>

<details>
  <summary>overide color scheme</summary>
  
  ```vim
  " overide color scheme 
  " autocmd ColorScheme * highlight StatusLine ctermfg=166 
  autocmd ColorScheme * highlight TabLineSel ctermfg=215 ctermbg=Black
  " autocmd ColorScheme * highlight DiffAdd ctermfg=Black ctermbg=166 
  " autocmd ColorScheme * highlight DiffChange ctermfg=Black ctermbg=160
  " autocmd ColorScheme * highlight DiffDelete ctermfg=Black ctermbg=160
  " autocmd ColorScheme * highlight DiffText ctermfg=Black ctermbg=160
  " autocmd ColorScheme * highlight Error ctermfg=160 ctermbg=Black cterm=NONE
  " autocmd ColorScheme * highlight StartifyNumber ctermfg=White 
  autocmd ColorScheme * highlight folded ctermbg=NONE ctermfg=240
  autocmd ColorScheme * highlight Comment ctermfg=White ctermbg=NONE
  " autocmd ColorScheme * highlight LineNr ctermbg=NONE
  " autocmd ColorScheme * highlight EndOfBuffer ctermfg=White
  " autocmd ColorScheme * highlight Normal ctermbg=NONE
  autocmd ColorScheme * highlight ModeMsg ctermfg=67 ctermbg=Black
  autocmd ColorScheme * highlight Underlined ctermfg=67 ctermbg=Black
  " autocmd ColorScheme * highlight Type ctermbg=NONE
  " autocmd ColorScheme * highlight Statement ctermbg=NONE ctermfg=Blue
  " autocmd ColorScheme * highlight Number ctermbg=NONE
  " autocmd ColorScheme * highlight Cursor ctermbg=166 ctermfg=White
  " autocmd ColorScheme * highlight CursorLine ctermbg=Grey ctermfg=White
  " autocmd ColorScheme * highlight CursorLineNr cterm=NONE ctermbg=Black ctermfg=166
  " autocmd ColorScheme * highlight String ctermbg=NONE
  " autocmd ColorScheme * highlight Special ctermbg=NONE
  " autocmd ColorScheme * highlight Title ctermbg=NONE
  " autocmd ColorScheme * highlight PreProc ctermbg=NONE
  " autocmd ColorScheme * highlight VertSplit ctermfg=166 ctermbg=235
  ```
</details>

<details>
  <summary>terminal cursor shape overide</summary>
  
  ```vim
  " terminal cursor shape overide 
  if $TERM_PROGRAM =~ "iTerm"
      let &t_SI = "\<Esc>]50;CursorShape=2\x7" " Underscore in insert mode
      let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
  endif
  ```
</details>

<details>
  <summary>pear tree</summary>
  
  ```vim
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
  \ '<html': {'closer': '></html>'},
  \ '<head': {'closer': '></head>'},
  \ '<title': {'closer': '></title>'},
  \ '<script': {'closer': '></script>'},
  \ '<body': {'closer': '></body>'},
  \ '<div': {'closer': '></div>'},
  \ '<h#': {'closer': '></h#>'},
  \ '<p': {'closer': '></p>'}
  \}

  " copy past tags and symbol pair 

  " \ '': {'closer': ''}
  " \ '<': {'closer': '></>'},
  ```
</details>

<details>
  <summary>spelling</summary>
  
  ```vim
  " spelling 
  set spelllang=en_us
  set spellfile=en.utf-8.add
  set complete+=kspell
  set secure
  ```
</details>

<details>
  <summary>wildmenu</summary>
  
  ```vim
  " wildmenu 
  set wildmenu
  set wildmode=list:longest
  set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx"
  ```
</details>

<details>
  <summary>ale</summary>
  
  ```vim
  " ale 
  set omnifunc=syntaxcomplete#Complete
  let g:ale_lint_on_enter = 0
  let g:ale_completion_enabled = 1
  let g:rustfmt_autosave = 1
  " set omnifunc=ale#completion#OmniFunc

  let g:ale_linters = {
  \    'bash': ['shellcheck'],
  \    'javascript': ['eslint'],
  \    'rust': ['cargo', 'rustfmt']
  \}

  let g:ale_fixers = {
  \    'bash': ['shellcheck'],
  \    'javascript': ['eslint'],
  \    'rust': ['rustfmt']
  \}

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
  ```
</details>

<details>
  <summary>ycm</summary>
  
  ```vim
  " ycm 
  
  ```
</details>

<details>
  <summary>vim template</summary>
  
  ```vim
  " vim template 
  " edit templates in ~/.vim/plugged/vim-templates/templates
  " WARNING PlugUpdate could cause loss of template configs store on git or
  " seprate txt file
  ```
</details>

<details>
  <summary>remappings</summary>
  
  ```vim
  " remappings current [0] 
  " remappinig for spell and suyntax error jumping same as / command's jumping n

  " noremap n ]s

  " remapping for para jump curly braces and horizontal word jump e and b  

  "noremap hjkl | hold 3 jumps | (h -> b) & (l -> e) & (j -> }) & (k -> {)
  ```
</details>

<details>
  <summary>macros</summary>
  
  ```vim
  " macros 

  " invoke via @reg_name

  " a register: shift i /*, shift a */
  " single line commenting /*txt*/

  " b register: shift i right-arrow2 del2, shift a del2
  " uncomment

  " multi linea and b: visual command auto-inserted-txt normal @reg_name
  
  " c register: visual } k zf
  ```
</details>

<details>
  <summary>save folds</summary>
  
  ```vim
  " save folds 
  augroup remember_folds
      autocmd!
      autocmd BufWinLeave *.* mkview!
      autocmd BufWinEnter *.* silent loadview
  augroup END
  ```
</details>

<details>
  <summary>default config</summary>
  
  ```vim
  " default config 
  colorscheme fahrenheit
  syntax on
  filetype on
  filetype plugin on
  filetype indent on
  set nocompatible
  set number
  set tabstop=4
  set shiftwidth=4
  set expandtab
  set backspace=2
  set autoindent
  set encoding=UTF-8
  set nowrap
  set cursorline
  set hidden
  set scrolloff=5
  set foldopen-=block
  set foldopen-=hor
  ```
</details>

<details>
  <summary>highlighting word searches with \</summary>
  
  ```vim
  " highlighting word searches with \ 
  set incsearch
  set hlsearch
  ```
</details>

<details>
  <summary>visual block increment</summary>
  
  ```vim
  " visual block increment 
  set nrformats=bin,alpha,octal,hex
  ```
</details>
