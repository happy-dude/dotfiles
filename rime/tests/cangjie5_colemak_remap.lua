package.path = 'rime/.local/share/fcitx5/rime/lua/?.lua;' .. package.path

local remap = require('cangjie5_colemak_remap')

local ACCEPTED, NOOP = 1, 2

-- The same physical keys, row by row, as each layout labels them. The
-- processor has to turn every Colemak label into the QWERTY label of that
-- key, which is the letter Cangjie assigns to it.
local qwerty = 'qwertyuiop' .. 'asdfghjkl;' .. 'zxcvbnm'
local colemak = 'qwfpgjluy;' .. 'arstdhneio' .. 'zxcvbkm'
assert(#qwerty == 27 and #colemak == #qwerty)

-- A key event with the given modifiers, e.g. event('f', { ctrl = true }).
local function event(key, modifiers)
  modifiers = modifiers or {}
  local function flag(name)
    return function()
      return modifiers[name] or false
    end
  end
  return {
    keycode = type(key) == 'number' and key or string.byte(key),
    release = flag('release'),
    alt = flag('alt'),
    ctrl = flag('ctrl'),
    super = flag('super'),
  }
end

-- Send one key to a fresh engine and return the processor's result and the
-- text it pushed into the composition.
local function press(key_event, options)
  options = options or {}
  local pushed = ''
  local context = {
    get_option = function(_, option)
      assert(option == 'ascii_mode', 'unexpected option ' .. option)
      return options.ascii_mode or false
    end,
    push_input = function(_, input)
      pushed = pushed .. input
    end,
  }
  local env = { engine = { context = context } }
  remap.init(env)
  return remap.func(key_event, env), pushed
end

local function check(label, key_event, options, expected_result, expected_pushed)
  local result, pushed = press(key_event, options)
  assert(
    result == expected_result,
    string.format('%s: expected result %d, got %s', label, expected_result, tostring(result))
  )
  assert(
    pushed == expected_pushed,
    string.format('%s: expected [%s] pushed, got [%s]', label, expected_pushed, pushed)
  )
end

for position = 1, #colemak do
  local typed = colemak:sub(position, position)
  check('Colemak ' .. typed, event(typed), nil, ACCEPTED, qwerty:sub(position, position))
end

-- Everything else is left to the processors after this one.
local passed_on = {
  { 'ASCII mode', event('f'), { ascii_mode = true } },
  { 'key release', event('f', { release = true }) },
  { 'Alt', event('f', { alt = true }) },
  { 'Control', event('f', { ctrl = true }) },
  { 'Super', event('f', { super = true }) },
  { 'uppercase letter', event('F') },
  { 'digit', event('1') },
  { 'space', event(' ') },
  { 'backtick', event('`') },
  { 'apostrophe', event("'") },
  { 'comma', event(',') },
  -- Keys outside Latin-1 have keycodes that string.char rejects.
  { 'Return', event(0xff0d) },
  { 'Left', event(0xff51) },
}
for _, case in ipairs(passed_on) do
  check(case[1], case[2], case[3], NOOP, '')
end
