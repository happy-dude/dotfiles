-- Quality-of-life tweaks for neovim written in lua
local qol = {}

-- https://old.reddit.com/r/neovim/comments/w0jzzv/smart_dd/

-- A plain dd on one blank line deletes into the black-hole register, so the
-- blank line does not replace the text last yanked. A count, including 1dd,
-- or a named register uses the stock dd: with 2dd the next line may hold
-- text, and a register is a request to keep the line. With 'clipboard' set,
-- v:register reports + or * instead of " for the default register, so an
-- explicit "+dd or "*dd cannot be told apart and also uses the black hole.
function qol.smart_dd()
  local register = vim.v.register
  local default_register = register == '"' or register == '+' or register == '*'
  if vim.v.count == 0 and default_register and vim.api.nvim_get_current_line():match('^%s*$') then
    return '"_dd'
  end
  return 'dd'
end

vim.keymap.set('n', 'dd', qol.smart_dd, { noremap = true, expr = true })

return qol
