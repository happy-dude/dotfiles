package.path = "rime/.local/share/fcitx5/rime/lua/?.lua;" .. package.path

local remap = require("cangjie5_colemak_remap")

local function event(keycode)
  return {
    keycode = keycode,
    release = function() return false end,
    alt = function() return false end,
    ctrl = function() return false end,
    super = function() return false end,
  }
end

local pushed = nil
local context = {
  get_option = function(_, option)
    assert(option == "ascii_mode")
    return false
  end,
  push_input = function(_, input)
    pushed = input
  end,
}
local env = { engine = { context = context } }

assert(remap.func(event(string.byte("f")), env) == 1)
assert(pushed == "e")

assert(remap.func(event(65293), env) == 2)
