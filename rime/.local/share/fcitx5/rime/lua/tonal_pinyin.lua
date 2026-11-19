-- tonal_pinyin.lua
-- Annotates Pinyin candidates: 長大 typed as zhangda shows
-- "zhǎng dà [zoeng2 daai6]".
--
-- readings.lua chooses each reading. The Pinyin follows the typed syllables;
-- nothing was typed for the Jyutping in brackets, so it follows the
-- dictionaries alone.

local readings = require('readings')

local function filter(input, env)
  for cand in input:iter() do
    local chars = readings.chars(cand.text or '')
    local typed = readings.typed_pinyin(cand.preedit, #chars)

    local parts = {}
    local mandarin = readings.mandarin(env.pinyin, env.luna, chars, typed)
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
