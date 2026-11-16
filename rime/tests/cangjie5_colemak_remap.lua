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

-- What the processor did with one key: its result, the text it pushed into
-- the composition, the keys it handed back to the engine, the text it
-- committed, and its result for each key that came back to it.
local function press(key_event, options)
  options = options or {}
  local seen = { pushed = '', forwarded = '', committed = '', reentered = {} }
  local env = {}
  local context = {
    get_option = function(_, option)
      assert(option == 'ascii_mode', 'unexpected option ' .. option)
      return options.ascii_mode or false
    end,
    push_input = function(_, input)
      seen.pushed = seen.pushed .. input
    end,
  }
  env.engine = {
    context = context,
    commit_text = function(_, text)
      seen.committed = seen.committed .. text
    end,
  }
  if options.process_key ~= 'missing' then
    -- Like the engine, run the key through the processors again, this one
    -- included, and say whether one of them took it.
    env.engine.process_key = function(_, forwarded)
      seen.forwarded = seen.forwarded .. forwarded.repr
      seen.reentered[#seen.reentered + 1] = remap.func(event(forwarded.repr), env)
      return options.process_key ~= 'unhandled'
    end
  end
  KeyEvent = function(repr)
    return { repr = repr }
  end
  remap.init(env)
  seen.result = remap.func(key_event, env)
  seen.env = env
  return seen
end

local function check(label, key_event, options, expected)
  local seen = press(key_event, options)
  for _, field in ipairs({ 'result', 'pushed', 'forwarded', 'committed' }) do
    local want = expected[field] or ''
    assert(
      seen[field] == want,
      string.format(
        '%s: expected %s [%s], got [%s]',
        label,
        field,
        tostring(want),
        tostring(seen[field])
      )
    )
  end
  -- A key handed back to the engine must not be remapped a second time.
  for _, result in ipairs(seen.reentered) do
    assert(result == NOOP, label .. ': the forwarded key was processed again')
  end
  assert(#seen.reentered == #seen.forwarded, label .. ': forwarded keys must come back once')
end

for position = 1, #colemak do
  local typed = colemak:sub(position, position)
  local meant = qwerty:sub(position, position)
  if meant == ';' then
    -- The semicolon key is no Cangjie letter. The engine gets it as a key,
    -- so that the speller or the punctuator handles it as on QWERTY.
    check('Colemak ' .. typed, event(typed), nil, { result = ACCEPTED, forwarded = ';' })
  else
    check('Colemak ' .. typed, event(typed), nil, { result = ACCEPTED, pushed = meant })
  end
end

-- No processor takes the semicolon when ASCII punctuation is on and nothing
-- is being composed. The application must not get the o that was pressed.
check(
  'semicolon nobody handled',
  event('o'),
  { process_key = 'unhandled' },
  { result = ACCEPTED, forwarded = ';', committed = ';' }
)

-- A librime-lua without Engine:process_key still separates codes.
check(
  'semicolon without process_key',
  event('o'),
  { process_key = 'missing' },
  { result = ACCEPTED, pushed = ';' }
)

-- Forwarding ends with the key, whether or not the engine could take it:
-- the next key is remapped as usual.
for _, process_key in ipairs({ 'handled', 'unhandled', 'missing' }) do
  local seen = press(event('o'), { process_key = process_key })
  seen.pushed = ''
  assert(remap.func(event(';'), seen.env) == ACCEPTED, 'key after a semicolon, ' .. process_key)
  assert(seen.pushed == 'p', 'key after a semicolon, ' .. process_key)
end

-- Everything else is left to the processors after this one.
local passed_on = {
  { 'ASCII mode', event('f'), { ascii_mode = true } },
  { 'ASCII mode, semicolon key', event('o'), { ascii_mode = true } },
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
  check(case[1], case[2], case[3], { result = NOOP })
end
