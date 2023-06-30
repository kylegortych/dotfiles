# dotfiles
Unix based directory struc 

<details>
<summary>fish</summary>

<details>
<summary>~/.config/fish/completions/temp.fish</summary>

```fish
#temp
```

</details>

<details>
<summary>~/.config/fish/completions/nix.fish</summary>

```fish
complete -c nix -x -a 'build log path-info registry why-depends daemon derivation hash key nar print-dev-env realisation show-config store doctor upgrade-nix'
```

</details>

<details>
<summary>~/.config/fish/conf.d/aliases.fish</summary>

```fish
# start of aliases

function fish-aliases --description "list all aliases"
  awk '/function /{print $2}' ~/.config/fish/conf.d/aliases.fish | column
end

function os-check-update --description "search for update"
  softwareupdate -l
end

function nix-ls --description "list nix packages"
  nix-env -q | column
end

# pkill verify pid match
# function terminate
#   ps | rg $argv && pkill $argv
# end

# concat commands with mktemp auto rm after termination?
# function network-info
#   ((ifconfig | rg "inet" | rg -v 127.0.0.1) && networksetup -listallhardwareports) | less
# end

function check-curl --description 'alias'
  curl $argv | less
end

function ls-env --description 'alias'
  env | column -t -s '='
end

function tips --description 'alias'
  less ~/.config/fish/Shell_Support/alias_script_support/tips.txt
end

function gpg-toggle
  gpg $argv 2>/dev/null &&
  set decrypted_file (string sub - 1 -4 $argv) &&
  chmod 600 $decrypted_file ||
  gpg -c --no-symkey-cache $argv && rm -f $argv
end

function py-current-pkgs --description 'alias'
  echo -n "pip                             pip3"\n\n; paste (pip list --not-required | psub) (pip3 list --not-required | psub)
end

function bun-list-g
  ls ~/.bun/install/global/node_modules
end

function npm-list-g
  npm list -g --depth=0
end

function py-ls --description 'alias'
  sed -n "/pip/,/Build Sys/{/Build Sys/!p;}" ~/.config/fish/Shell_Support/alias_script_support/build_sys.txt
end

function weather --description 'alias'
  curl wttr.in/$argv
end

function weather-radar --description 'alias'
  mpv $argv
end

function weather-radar-local --description 'alias'
  mpv https://radar.weather.gov/ridge/lite/NORTHEAST_loop.gif
end

function check-stockmarket --description 'alias'
  curl terminal-stocks.shashi.dev/$argv
end

function check-news --description 'alias'
  curl getnews.tech/$argv || curl getnews.tech
end

function open-exit --description 'alias'
  open -a "$argv" && exit
end

function apl-run-script --description 'alias'
  apl --noSV --noColor --noCIN -q -f $argv
end

function verilog-compile --description 'alias'
  iverilog -o $argv
end

function verilog-run --description 'alias'
  vvp $argv
end

function jdk-ls --description "list jdk versions"
  /usr/libexec/java_home -V
end

function jdk-delta --description "list jdk versions"
  /usr/libexec/java_home -v $argv
end

function doom --description 'alias'
  ~/.emacs.d/bin/doom $argv
end

# function kotlin-run-script --description 'alias'
#   kotlinc $argv -include-runtime -d $argv && java -jar $argv
# end

# git aliases
function git-reset --description "reset git prj ie reclone"
  git stash -u && git stash drop
end

#python envar
#set -x PYTHONSTARTUP "/Users/kylegortych/.config/python/conf.py"
```

</details>

</details>

<details>
<summary>nvim</summary>

<details>
<summary>~/.config/nvim/init.lua</summary>

```lua
--[[
     _       _ __    __
    (_)___  (_) /_  / /_  ______ _
   / / __ \/ / __/ / / / / / __ `/
  / / / / / / /__ / / /_/ / /_/ /
 /_/_/ /_/_/\__(_)_/\__,_/\__,_/

 Maintainer: Kyle Gortych
 github:     https://github.com/KyleGortych
 Linkedin:   https://www.linkedin.com/in/kyle-gortych-163449240

 DESC:               config for nvim
 DEPENDENCIES:       nvim ± v0.9
 Date Last Modified: 01/16/2023
--]]

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
  -- other plugins
  "widatama/vim-phoenix",
  "tanvirtin/monokai.nvim",
  "rockerBOO/boo-colorscheme-nvim",
  "tmsvg/pear-tree",
  "romainl/vim-cool",
  "mfussenegger/nvim-dap",
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {                                      -- Optional
        'williamboman/mason.nvim',
        build = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-nvim-lsp',
      'saadparwaiz1/cmp_luasnip',
      'L3MON4D3/LuaSnip',
      'rafamadriz/friendly-snippets',
    }
  }
}

require("lazy").setup(plugins)

-- terminal cursor shape overide
vim.cmd([[
  augroup RestoreCursorShapeOnExit
    autocmd!
    autocmd VimLeave * set guicursor=a:hor20-blinkwait400-blinkoff400-blinkon250
  augroup END
]])

-- save folds
-- conflict | :diffoff! wont work
-- workaround | manual reset :mkview :loadview
vim.cmd([[
  augroup remember_folds
      autocmd!
      autocmd BufWinLeave *.* mkview
      autocmd BufWinEnter *.* silent! loadview
  augroup END
]])

-- fn | if max file column > n and max line num > n
-- :NoMatchParen | set nocul | set lz | set smc=80

-- Custom Commands
-- ie. like shell alisases

-- lang Independent Formater
vim.cmd([[command! -nargs=0 Format :%s/\s\+$//e]])
vim.cmd([[command! -nargs=0 Formatjson :%!jq]])

-- all formats concatinated
-- Format :%s/\s\+$//e | %s/{/ {/g
-- Format custom args
-- Trailing White Spaces :%s/\s\+$//e
-- Space Before {        :%s/{/ {/g or handeld by space before and after operators?
-- Column Indentation    : or use vim par

-- for file type json then Format_formatername

-- Format insert-delimiter
-- create args | range and delimiter
-- remove spaces between replace with delimiter ie. CLI aliase brew-pkgs :3,23 s/\s\+/|/g

-- remappings
--  xnoremap <some_key> :'<,'>!column -t

--macros

--invoke single line via @letter

--to clear use :let @letter = ''

--multi line: shift v command
--result: :'<,'>normal @letter

--Outline
--@letter:      comands
--macro result: result

--@a:
--macro result:

-- non confilicing remappings
--remappinig for spell and suyntax error jumping same as / command's jumping n
--noremap n ]s

local lsp = require('lsp-zero')
lsp.preset('recommended')

vim.diagnostic.config({
  virtual_text = false,
  signs = true,
  update_in_insert = true,
  underline = true,
  severity_sort = false,
  float = true,
})

vim.o.updatetime = 250
vim.cmd [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]

local cmp = require('cmp')
local cmp_action = require('lsp-zero').cmp_action()
require('luasnip.loaders.from_vscode').lazy_load()

cmp.setup({
  sources = {
    {name = 'nvim_lsp'},
    {name = 'luasnip'},
  },
  mapping = {
    ['<CR>'] = cmp.mapping.confirm({select = false}),
    ['<Tab>'] = cmp_action.luasnip_supertab(),
    ['S-Tab'] = cmp_action.luasnip_shift_supertab(),
  }
})


lsp.nvim_workspace()
lsp.setup()

require("mason").setup({
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "…",
            package_uninstalled = "✗"
        }
    }
})

require("mason-lspconfig").setup()

-- pear tree
vim.cmd([[
let g:['pear_tree_pairs'] = {
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
  \ }
]])

-- overide colorscheme
vim.cmd([[
  augroup OverideColor
    autocmd!
    autocmd ColorScheme * hi Folded guifg=#191919 guibg=#6a6a6a
  augroup END
]])

-- main defaults
vim.cmd('colorscheme phoenix')
vim.opt.background = 'dark'
vim.opt.termguicolors = true
vim.cmd('syntax on')
vim.cmd('filetype plugin indent on')
vim.cmd('set clipboard+=unnamedplus')
vim.opt.number = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.cmd('set backspace=2')
vim.opt.autoindent = true
vim.opt.encoding='UTF-8'
vim.opt.wrap = false
vim.opt.tw = 45
vim.opt.lz = true
vim.opt.tf = true
vim.cmd([[set fo=cq]])
vim.opt.cul = true
vim.opt.hidden = true
vim.opt.scrolloff = 5
vim.opt.foldopen:remove 'block'
vim.opt.foldopen:remove 'hor'
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.nrformats = 'bin,alpha,octal,hex'
vim.opt.spell = false
vim.opt.spelllang = 'en_us'
vim.opt.spellfile = 'en.utf-8.add'
vim.opt.complete:append 'kspell'
vim.cmd([[set completeopt=menu,menuone,noselect]])
vim.opt.secure = true
vim.opt.wildmenu = true
vim.opt.wildmode = 'list:longest'
vim.opt.wildignore = '*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx"'
vim.opt.signcolumn = 'yes'
vim.opt.guicursor = 'i:hor20-blinkwait400-blinkoff400-blinkon250'

vim.g['netrw_winsize'] = 20
vim.g.netrw_liststyle = 3
vim.g.netrw_altv = 1

vim.o.statusline = "%#Directory# %m %f %= gqfmt:[%{&fo}] pos:%l:%c"vim.opt.termguicolors = true
```

</details>

</details>

<details>
<summary>Doom Emacs</summary>

<details>
<summary>~/.config/doom/config.el</summary>

```elisp
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'afternoon)
(setq doom-theme 'klere)
;;(setq doom-theme 'darkokai)
;;(add-to-list 'default-frame-alist '(background-mode . dark))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; start of my config

;; initial defualt inline org images
;; (setq org-image-actual-width '100)
(setq org-image-actual-width nil)

;; screen size
;;(add-hook 'window-setup-hook #'toggle-frame-maximized)
(setq initial-frame-alist '((top . 1) (right . 1) (width . 130) (height . 100)))

;; font
(setq doom-font (font-spec :family "TerminessTTF Nerd Font Mono" :size 16))

;; dashboard
;;(setq fancy-splash-image "/Users/kylegortych/Downloads/doom-emacs-bw-light.svg")

(defun skull ()
  (let* ((banner '("   .o oOOOOOOOo                                            OOOo    "
                   "   Ob.OOOOOOOo  OOOo.      oOOo.                      .adOOOOOOO   "
                   "   OboO000000000000.OOo. .oOOOOOo.    OOOo.oOOOOOo..0000000000OO   "
                   "   OOP.oOOOOOOOOOOO iPOOOOOOOOOOOo.   `iOOOOOOOOOP,OOOOOOOOOOOB'   "
                   "   `O'OOOO'     `OOOOo'OOOOOOOOOOO` .adOOOOOOOOO'oOOO'    `OOOOo   "
                   "   .OOOO'            `OOOOOOOOOOOOOOOOOOOOOOOOOO'            `OO   "
                   "   OOOOO                 'iOOOOOOOOOOOOOOOOi`                oOO   "
                   "  oOOOOOba.                .adOOOOOOOOOOba               .adOOOOo. "
                   " oOOOOOOOOOOOOOba.    .adOOOOOOOOOO@^OOOOOOOba.     .adOOOOOOOOOOOO"
                   "OOOOOOOOOOOOOOOOO.OOOOOOOOOOOOOO'`  ''OOOOOOOOOOOOO.OOOOOOOOOOOOOO "
                   "'OOOO'       'YOoOOOOMOIONODOO'`  .   ''OOROAOPOEOOOoOY'     'OOO' "
                   "   Y           'OOOOOOOOOOOOOO: .oOOo. :OOOOOOOOOOO?'         :`   "
                   "   :            .oO%OOOOOOOOOOo.OOOOOO.oOOOOOOOOOOOO?         .    "
                   "   .            oOOPi%OOOOOOOOoOOOOOOO?oOOOOO?OOOOiOOo             "
                   "                '%o  OOOO'%OOOO%'%OOOOO'OOOOOO'OOO':               "
                   "                     `$i  `OOOO' `O'Y ' `OOOO'  o             .    "
                   "   .                  .     OP'          : o     .                 "
                   "                             :                                     "
                   "                             .                                     "
                   "                                                                   "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 68)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'skull)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "config by Kyle Gortych")))
;; apply icons to dired?


;; disable quit comfirmation
(setq confirm-kill-emacs nil)

;; cursor shape
(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        )

(setq evil-insert-state-cursor '(hbar "white")
      evil-normal-state-cursor '(box "white")
      evil-motion-state-cursor '(box "white")
      evil-visual-state-cursor '(box "white")
      evil-emacs-state-cursor '(box "white"))

;; note: org export html | dosn't include superstar | css file to change behavior

;; orgmode bullets
(setq org-superstar-headline-bullets-list '("➀" "➁" "➂" "➃" "➄"))

;; orgmode list symbol
(setq org-superstar-item-bullet-alist '((?+ . ?») (?- . ?») (?➤ . ?»)))

;; stripe-buffer
;; (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
;; (add-hook 'dired-mode-hook 'stripe-listify-buffer)
;; (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
```

</details>

<details>
<summary>~/.config/doom/packages.el</summary>

```elisp
;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! evil-terminal-cursor-changer)
(package! klere-theme)
(package! darkokai-theme)
;; (package! stripe-buffer)
;;(package! )

;;(package! example :recipe
;;  (:host github
;;   :repo "url"
;;   :files ("file-name.el" "url raw?"))
```

</details>

<details>
<summary>~/.config/doom/init.el</summary>

```elisp
;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life
       ;;vertico           ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       ;;doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       ;;dired           ; making dired pretty [functional]
       (dired +icons)    ; dired with all-the-icons pkg
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell) ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio            ; Writes a PhD for you (citation needed)
       debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       (go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       (haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       json              ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       java
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;;org               ; organize your plain life in plain text
       (org +pretty)       ; org with superstar bullets pkg
       php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web               ; the tubes
       yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
```

</details>

</details>
  
<details>
<summary>nixos</summary>

<details>
<summary>/etc/nixos/configuration.nix</summary>
    
```nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.timeout = 2;

  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.network-setup.enable = false;

  #fileSystems."/nix".options = [ "noatime" ];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  networking.hostName = ""; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # environment.shells = with pkgs; [ fish ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.blank = {
    isNormalUser = true;
    description = "Kyle Gortych";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    packages = with pkgs; [
      #gui
      firefox
      thunderbird
      libreoffice
      bitwarden
      zoom-us
      kdenlive
      blender
      freecad
      kicad
      logisim-evolution
      virtualbox
      timeshift
      jetbrains.idea-community
      emacs

      #cli
      calcurse
      btop
      ripgrep
      figlet
      neofetch
      starship
      wl-clipboard

      #lang
      python311
      python311Packages.datetime
      python311Packages.ptpython
    ];
  };

  home-manager.users.blank = { pkgs, ... }: {
    home.stateVersion = "23.05";
    programs.neovim = {
      enable = true;
      extraLuaConfig = ''
        vim.cmd [[colorscheme habamax]]
        vim.opt.number = true
      '';
    };
    programs.wezterm = {
      enable = true;
      extraConfig = ''
        local wezterm = require("wezterm")
        return {
          font = wezterm.font 'ShureTechMono Nerd Font',
          font_size = 16.0,
          default_cursor_style = 'BlinkingBlock',
          color_scheme = 'Monokai Remastered',
          colors = {
            background = '#191919',
          }
        }
      '';
    };
    home.packages = with pkgs; [
      w3m
    ];
  };

  

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting
      fish_vi_key_bindings
      starship init fish | source
    '';
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "ShareTechMono" ]; })
  ];

  services.clamav = {
    daemon.enable = true;
    # updater.enable = true;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  hardware.system76.enableAll = true;

}
```
    
</details>  
  
</details>
    
