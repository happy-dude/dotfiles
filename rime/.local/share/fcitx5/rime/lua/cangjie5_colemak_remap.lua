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
  q = "q", w = "w", f = "e", p = "r", g = "t", j = "y", l = "u", u = "i", y = "o", [";"] = "p",
  -- Row 2: A S D F G H J K L ;
  a = "a", r = "s", s = "d", t = "f", d = "g", h = "h", n = "j", e = "k", i = "l", o = ";",
  -- Row 3: Z X C V B N M
  z = "z", x = "x", c = "c", v = "v", b = "b", k = "n", m = "m"
}

local function init(env)
  -- Nothing to initialize
end

local function func(key_event, env)
  local engine = env.engine
  local context = engine.context
  local key = key_event.keycode

  -- Only process when not in ASCII mode
  if context:get_option("ascii_mode") then
    return 2  -- kNoop
  end

  -- Only process key press (not release), no modifiers
  if key_event:release() or key_event:alt() or key_event:ctrl() or key_event:super() then
    return 2  -- kNoop
  end

  -- Get the character representation
  local ch = string.char(key)

  -- Only process lowercase letters and semicolon
  if ch and ch:match("^[a-z;]$") then
    local remapped = colemak_to_qwerty[ch]
    if remapped then
      context:push_input(remapped)
      return 1  -- kAccepted
    end
  end

  return 2  -- kNoop
end

return { init = init, func = func }
