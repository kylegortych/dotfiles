# dotfiles
sys config files for various applications

## nvim
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

-- paq conf
-- require "paq" {
-- 
--   -- cosmetic --
--   -- "dylanaraps/wal.vim";
--   "jreybert/vimagit";
--   "tmsvg/pear-tree";
--   "romainl/vim-cool";
--   {"nvim-treesitter/nvim-treesitter", run=function() vim.cmd "TSUpdate" end};
--   "widatama/vim-phoenix";
--   "tanvirtin/monokai.nvim";
--   -- "erichdongubler/vim-sublime-monokai";
--   "rockerBOO/boo-colorscheme-nvim";
-- 
--   -- lsp --
--   "VonHeikemen/lsp-zero.nvim";
--   "neovim/nvim-lspconfig";
--   "williamboman/mason.nvim";
--   "williamboman/mason-lspconfig.nvim";
-- 
--   -- completion --
--   'hrsh7th/nvim-cmp';
--   'hrsh7th/cmp-buffer';
--   'hrsh7th/cmp-path';
--   'saadparwaiz1/cmp_luasnip';
--   'hrsh7th/cmp-nvim-lsp';
--   'hrsh7th/cmp-nvim-lua';
--   
--   -- Snippets
--   'L3MON4D3/LuaSnip';
--   'rafamadriz/friendly-snippets';
--   
-- }

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

## Doom Emacs
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


