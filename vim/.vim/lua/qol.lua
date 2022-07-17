-- Quality-of-life tweaks for neovim written in lua
local qol = {}

-- https://old.reddit.com/r/neovim/comments/w0jzzv/smart_dd/

function qol.smart_dd()
  if vim.api.nvim_get_current_line():match("^%s*$") then
    return "\"_dd"
  else
    return "dd"
  end
end

vim.keymap.set( "n", "dd", qol.smart_dd, { noremap = true, expr = true } )

return qol
