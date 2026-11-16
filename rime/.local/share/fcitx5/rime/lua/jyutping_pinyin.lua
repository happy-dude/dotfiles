-- jyutping_pinyin.lua
-- Appends tonal Pinyin to Jyutping candidates: 長大 typed as zoengdaai shows
-- "zoeng2 daai6 [zhǎng dà]".
--
-- The Jyutping before the bracket is the stock schema's and follows what
-- was typed. Nothing was typed for the Pinyin, so readings.lua chooses it
-- from the dictionaries alone.

local readings = require('readings')
local romanization = require('romanization')

local function filter(input, env)
  for cand in input:iter() do
    local chars = readings.chars(cand.text or '')
    local mandarin = readings.join(env.pinyin:resolve(chars), #chars, romanization.convert_tones)

    if mandarin then
      local existing = cand.comment or ''
      if existing ~= '' then
        cand.comment = existing .. ' [' .. mandarin .. ']'
      else
        cand.comment = mandarin
      end
    end

    yield(cand)
  end
end

local function init(env)
  local ok, reverse = pcall(ReverseLookup, 'terra_pinyin')
  env.pinyin = readings.resolver(ok and reverse or nil, readings.tables().pinyin)
end

return { init = init, func = filter }
