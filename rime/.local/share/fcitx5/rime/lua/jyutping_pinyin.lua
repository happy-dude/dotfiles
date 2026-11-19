-- jyutping_pinyin.lua
-- Appends tonal Pinyin to Jyutping candidates: 長大 typed as zoengdaai shows
-- "zoeng2 daai6 [zhǎng dà]".
--
-- In ordinary Cantonese input nothing was typed for the Pinyin. Behind
-- the backtick the luna_pinyin translator supplies a Mandarin preedit;
-- the affix segmentor has already separated the prefix and optional suffix.

local readings = require('readings')

local function filter(input, env)
  for cand in input:iter() do
    local chars = readings.chars(cand.text or '')
    local typed = env.mandarin_input and readings.typed_pinyin(cand.preedit, #chars)
    local mandarin = readings.mandarin(env.pinyin, env.luna, chars, typed)

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

-- Rime attaches filters to individual segment menus, which may be consumed
-- later. Select the instance by segment tag, not the current context input.
return function(mandarin_input)
  return {
    tags_match = function(segment)
      return segment:has_tag('luna_pinyin') == mandarin_input
    end,
    init = function(env)
      env.mandarin_input = mandarin_input
      local ok, reverse = pcall(ReverseLookup, 'terra_pinyin')
      env.pinyin = readings.resolver(ok and reverse or nil, readings.tables().pinyin)
      if mandarin_input then
        ok, reverse = pcall(ReverseLookup, 'luna_pinyin')
        env.luna = ok and reverse or nil
      end
    end,
    func = filter,
  }
end
