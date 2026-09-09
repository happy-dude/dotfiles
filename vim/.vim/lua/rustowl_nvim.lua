-- Keep this module name distinct from the RustOwl plugin's `rustowl` module.
require('rustowl').setup({
  auto_attach = true,
  auto_enable = true,
  idle_time = 500,
  client = {
    cmd = { 'rustowl', '--stdio' },
  },
  highlight_styles = {
    definitely_live = 'undercurl',
    maybe_initialized = 'undercurl',
    imm_borrow = 'undercurl',
    mut_borrow = 'undercurl',
    move = 'undercurl',
    call = 'undercurl',
    shared_mut = 'undercurl',
    outlive = 'undercurl',
  },
  colors = {
    definitely_live = '#00cc00',
    imm_borrow = '#0000cc',
    mut_borrow = '#cc00cc',
    move = '#cccc00',
    call = '#cccc00',
    outlive = '#cc0000',
  },
})
