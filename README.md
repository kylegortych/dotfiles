<div align="right">
 
![main](https://github.com/kylegortych/dotfiles/actions/workflows/main.yml/badge.svg) <!-- ![original](https://github.com/kylegortych/dotfiles/actions/workflows/original.yml/badge.svg?branch=original) -->

</div>

# My Dotfiles
Based on NixOS file system

<br>

<details>
<summary>old nvim config</summary>

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
    
<div align="center">
 
### :hammer_and_wrench: Tools :

| Version Control | Build System | Languages |
| --------------- | ------------ | --------- |
| <img src="https://img.shields.io/badge/Git-white?style=plastic&logo=git&logoColor=red" title="Git" alt="Git" height="30"/> | <img src="https://img.shields.io/badge/nix-white?style=plastic&logo=nixos" title="nix" alt="nix" height="30"/> | <img src="https://custom-icon-badges.demolab.com/badge/nix-white.svg?&sytle=plastic&logo=nixos" title="nix" alt="nix" height="30"/> |
</div>
<br>

### Nix Commands

* `./` - nix build
<br>

<a href="your-gmail-link?">:mailbox:</a> How to reach the maintainer
