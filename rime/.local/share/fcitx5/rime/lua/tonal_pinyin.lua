-- tonal_pinyin.lua
-- Annotates Pinyin candidates: 長大 typed as zhangda shows
-- "zhǎng dà [zoeng2 daai6]".
--
-- readings.lua chooses each reading. The Pinyin follows the typed syllables;
-- nothing was typed for the Jyutping in brackets, so it follows the
-- dictionaries alone.

local readings = require('readings')
local romanization = require('romanization')

-- luna_pinyin has no tones and spells ü as v.
local function toneless_pinyin(code)
  return (code:gsub('v', 'ü'))
end

-- Tonal Pinyin for every character, keyed by position. terra_pinyin has the
-- tones but fewer readings than luna_pinyin, the dictionary the candidates
-- come from: 撂 can be typed lve, which terra_pinyin does not list. A typed
-- syllable only luna_pinyin knows is shown without a tone instead of as
-- another reading with one.
local function pinyin(env, chars, typed)
  local tonal = env.pinyin:resolve(chars, typed)
  local shown = {}
  for position, char in ipairs(chars) do
    local syllable = typed and typed[position]
    local code = tonal[position]
    if code and not (syllable and readings.toneless(code) ~= syllable) then
      shown[position] = romanization.convert_tones(code)
    else
      local plain = readings.codes(env.luna, char)
      local spelled = syllable and readings.spelled(plain, syllable)
      if spelled then
        shown[position] = toneless_pinyin(spelled)
      elseif code then
        shown[position] = romanization.convert_tones(code)
      elseif plain[1] then
        shown[position] = toneless_pinyin(plain[1])
      end
    end
  end
  return shown
end

local function filter(input, env)
  for cand in input:iter() do
    local chars = readings.chars(cand.text or '')
    local typed = readings.typed_pinyin(cand.preedit, #chars)

    local parts = {}
    local mandarin = readings.join(pinyin(env, chars, typed), #chars)
    if mandarin then
      parts[#parts + 1] = mandarin
    end
    local cantonese = readings.join(env.jyutping:resolve(chars), #chars)
    if cantonese then
      parts[#parts + 1] = '[' .. cantonese .. ']'
    end

    if #parts > 0 then
      local annotation = table.concat(parts, ' ')
      local existing = cand.comment or ''
      if existing ~= '' then
        cand.comment = existing .. ' ' .. annotation
      else
        cand.comment = annotation
      end
    end

    yield(cand)
  end
end

local function reverse_lookup(dictionary)
  local ok, reverse = pcall(ReverseLookup, dictionary)
  return ok and reverse or nil
end

local function init(env)
  local tables = readings.tables()
  env.pinyin = readings.resolver(reverse_lookup('terra_pinyin'), tables.pinyin)
  env.luna = reverse_lookup('luna_pinyin')
  env.jyutping = readings.resolver(reverse_lookup('jyut6ping3'), tables.jyutping)
end

return { init = init, func = filter }
