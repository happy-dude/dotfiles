-- Keep this module name distinct from the RustOwl plugin's `rustowl` module.
require('rustowl').setup({
  auto_attach = true,
  auto_enable = true,
  idle_time = 500,
  client = {
    cmd = { 'rustowl', '--stdio' },
  },
  highlight_style = 'undercurl',
  colors = {
    lifetime = '#00cc00',
    imm_borrow = '#0000cc',
    mut_borrow = '#cc00cc',
    move = '#cccc00',
    call = '#cccc00',
    outlive = '#cc0000',
  },
})
