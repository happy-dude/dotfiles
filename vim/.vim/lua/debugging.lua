local dap = require('dap')

dap.adapters.lldb = {
  type = 'executable',
  command = '/usr/bin/lldb-vscode-14', -- adjust as needed, must be absolute path
  name = 'lldb',
}

local c_debug = {
  name = 'Launch lldb',
  type = 'lldb',
  request = 'launch',
  program = function ()
    return vim.fn.input(
      'Path to executable: ',
      vim.fn.getcwd() .. '/',
      'file'
    )
  end,
  cwd = '${workspaceFolder}',
  stopOnEntry = false,
  args = {},
  runInTerminal = false,
}

dap.configurations.c = { c_debug }
dap.configurations.cpp = dap.configurations.c

vim.keymap.set('n', '<F5>',       function() require('dap').continue() end)
vim.keymap.set('n', '<F10>',      function() require('dap').step_over() end)
vim.keymap.set('n', '<F11>',      function() require('dap').step_into() end)
vim.keymap.set('n', '<F12>',      function() require('dap').step_out() end)
vim.keymap.set('n', '<Leader>b',  function() require('dap').toggle_breakpoint() end)
vim.keymap.set('n', '<Leader>B',  function() require('dap').set_breakpoint() end)
vim.keymap.set('n', '<Leader>lp', function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)
vim.keymap.set({'n', 'v'}, '<Leader>dh', function()
  require('dap.ui.widgets').hover()
end)
vim.keymap.set({'n', 'v'}, '<Leader>dp', function()
  require('dap.ui.widgets').preview()
end)
vim.keymap.set('n', '<Leader>df', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.frames)
end)
vim.keymap.set('n', '<Leader>ds', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.scopes)
end)

local dapui = require('dapui')
require("dapui").setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end
