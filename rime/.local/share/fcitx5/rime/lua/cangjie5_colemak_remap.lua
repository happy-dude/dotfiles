-- cangjie5_colemak_remap.lua
-- Remaps Colemak input to QWERTY for Cangjie

-- Colemak character received → QWERTY character expected
-- (QWERTY keyboard order, top to bottom, left to right)
--
-- Physical pos (QWERTY) → Colemak sends → QWERTY expected
-- Row 1: Q W E R T Y U I O P  →  q w f p g j l u y ;  →  q w e r t y u i o p
-- Row 2: A S D F G H J K L ;  →  a r s t d h n e i o  →  a s d f g h j k l ;
-- Row 3: Z X C V B N M        →  z x c v b k m        →  z x c v b n m

local colemak_to_qwerty = {
  -- Row 1: Q W E R T Y U I O P
  q = 'q',
  w = 'w',
  f = 'e',
  p = 'r',
  g = 't',
  j = 'y',
  l = 'u',
  u = 'i',
  y = 'o',
  [';'] = 'p',
  -- Row 2: A S D F G H J K L ;
  a = 'a',
  r = 's',
  s = 'd',
  t = 'f',
  d = 'g',
  h = 'h',
  n = 'j',
  e = 'k',
  i = 'l',
  o = ';',
  -- Row 3: Z X C V B N M
  z = 'z',
  x = 'x',
  c = 'c',
  v = 'v',
  b = 'b',
  k = 'n',
  m = 'm',
}

-- What a lua_processor returns: kAccepted, and kNoop to pass the key on.
local ACCEPTED, NOOP = 1, 2

local function init(env)
  -- Nothing to initialize
end

-- Colemak o is on the QWERTY semicolon key, which is no Cangjie letter: it
-- separates codes while composing and types punctuation otherwise, and the
-- speller and the punctuator decide which. So the key is not pushed as input
-- like the letters; the engine gets a real semicolon to run through its
-- processors, this one included, which is why func() looks at
-- env.forwarding first.
local function semicolon(env)
  local engine = env.engine
  env.forwarding = true
  local ok, handled = pcall(function()
    return engine:process_key(KeyEvent(';'))
  end)
  env.forwarding = false
  if not ok then
    -- This librime-lua cannot hand the engine a key. Pushing the character
    -- still separates codes; with nothing being composed it leaves ； as a
    -- candidate to confirm instead of typing it.
    engine.context:push_input(';')
  elseif not handled then
    -- No processor took it: ASCII punctuation is on. Stock Cangjie leaves
    -- the key to the application, which here would get the o that was
    -- pressed, so type the semicolon for it.
    engine:commit_text(';')
  end
  return ACCEPTED
end

local function func(key_event, env)
  if env.forwarding then
    return NOOP
  end

  local engine = env.engine
  local context = engine.context
  local key = key_event.keycode

  -- Only process when not in ASCII mode
  if context:get_option('ascii_mode') then
    return NOOP
  end

  -- Leave releases and Alt/Ctrl/Super shortcuts to later processors.
  if key_event:release() or key_event:alt() or key_event:ctrl() or key_event:super() then
    return NOOP
  end

  -- Key events also include non-byte special keys such as Return and arrows.
  -- Avoid converting those with string.char before checking the range.
  if (key < string.byte('a') or key > string.byte('z')) and key ~= string.byte(';') then
    return NOOP
  end

  local remapped = colemak_to_qwerty[string.char(key)]
  if remapped == ';' then
    return semicolon(env)
  elseif remapped then
    context:push_input(remapped)
    return ACCEPTED
  end

  return NOOP
end

return { init = init, func = func }
