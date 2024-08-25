{ config, pkgs, ... }:

{
  imports =
    [ 
      <home-manager/nixos>
    ];

  home-manager.users.blank = { pkgs, ... }: {
    programs.neovim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        nvim-lspconfig {
          plugin = nvim-lspconfig;
          type = "lua";
          config = "
            require'lspconfig'.pyright.setup{}
            require'lspconfig'.lua_ls.setup{}
            -- require'lspconfig'.jdtls.setup{}
            -- require'lspconfig'.nil.setup{}
          ";
        }
        vim-monokai {
          plugin = vim-monokai;
          #type = "lua"
          #config = " ";
        }
        #nvim-cmp {
        #  plugin = nvim-cmp;
        #  type = "lua"
        #  #config = "";
        #}
        nvim-dap {
          plugin = nvim-dap;
          #type = "lua"
          #config = ""; 
        }
        pear-tree { 
          plugin = pear-tree; 
          #type = "lua"
          #config = ""; 
        }
        vim-cool { 
          plugin = vim-cool; 
          #type = "lua"
          #config = ""; 
        }
      ];
      extraLuaConfig = ''
        -- vim.cmd([[
        --   augroup SaveFolds
        --     autocmd!
        --     autocmd BufWinLeave * mkview
        --   augroup END
        -- ]])

        -- vim.cmd([[
        --   augroup LoadFolds
        --     autocmd!
        --     autocmd BufWinEnter * silent! loadview
        --   augroup END
        -- ]])

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

        vim.cmd [[colorscheme monokai]]
        vim.cmd [[set clipboard+=unnamedplus]]
        vim.g.netrw_winsize = 25
        vim.g.netrw_liststyle = 3
        vim.opt.number = true
        vim.opt.cursorline = true
        vim.opt.tabstop = 2
        vim.opt.shiftwidth = 2
        vim.opt.expandtab = true
        vim.opt.softtabstop = 2
      '';
    };
  };
}
