-- nvim-treesitter config

local api = vim.api
local parsers = require 'nvim-treesitter.parsers'

require'nvim-treesitter.install'.compilers = { "clang" }

require 'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",    -- one of "all", "language", or a list of languages

    -- nvim-treesitter native modules
    highlight = {
        enable  = true,             -- false will disable the whole extension
        -- disable = { 'commonlisp' }  -- list of language that will be disabled
        -- custom_captures = { }    -- custom highlight groups for captures
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection      = "gi",
            node_incremental    = "n",
            node_decremental    = "N",
            scope_incremental   = "b",
        },
    },

    -- external modules
    textobjects = {
        select = {
            enable = true,
            keymaps = {
                -- capture groups defined in textobjects.scm
                ["fo"] = "@function.outer",
                ["fi"] = "@function.inner",
                ["co"] = "@class.outer",
                ["ci"] = "@class.inner",
            },
        },
        lsp_interop =               { enable = false },
        move =                      { enable = false },
        swap =                      { enable = false },
    },
    refactor = {
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = "gs",
            },
        },
        highlight_current_scope =   { enable = false },
        highlight_definitions =     { enable = false },     -- provided by coc.nvim
        navigation =                { enable = false },
    },
    rainbow = {
      enable = true,
      extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int
      colors = {
          "#FE2712",
          "#66B032",
          "#0392CE",
          "#FD5308",
          "#0247FE",
          "#FB9902",
          "#8601AF",
          "#FEFE33",
          "#A7194B",
          "#D0EA2B",
      },
      termcolors = {
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "9",
          "10",
          "11" ,
          "12",
          "13",
          "14",
          "15",
      },
    },
}

-- turn on treesitter folding for supported languages
-- TODO: submit a patch that enables folding as a nvim-treesitter module
local ft_str = ""
local autocmd_fold_str = ""

for _, ft in pairs(parsers.available_parsers()) do
    if (ft == "c_sharp") then
        ft_str = ft_str..'cs'..','
    else
        ft_str = ft_str..ft..','
    end
end

autocmd_fold_str = 'autocmd Filetype '..ft_str..' setlocal foldmethod=expr foldexpr=nvim_treesitter#foldexpr()'
api.nvim_command(autocmd_fold_str)

