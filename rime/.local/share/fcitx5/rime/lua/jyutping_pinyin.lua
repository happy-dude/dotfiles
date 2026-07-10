-- jyutping_pinyin.lua
-- Adds tonal pinyin annotations to jyutping candidates

local romanization = require('romanization')

local function filter(input, env)
  for cand in input:iter() do
    local text = cand.text

    if text and text ~= '' and env.terra then
      local parts = {}

      for p, c in utf8.codes(text) do
        local char = utf8.char(c)
        local ok, code = pcall(function()
          return env.terra:lookup(char)
        end)
        if ok and code and code ~= '' then
          local first = code:match('^([^\t ]+)')
          if first then
            local converted = romanization.convert_tones(first)
            if converted then
              table.insert(parts, converted)
            end
          end
        end
      end

      if #parts > 0 then
        -- Append pinyin to existing jyutping comment
        local existing = cand.comment or ''
        if existing ~= '' then
          cand.comment = existing .. ' [' .. table.concat(parts, ' ') .. ']'
        else
          cand.comment = table.concat(parts, ' ')
        end
      end
    end

    yield(cand)
  end
end

local function init(env)
  local ok, rev = pcall(ReverseLookup, 'terra_pinyin')
  env.terra = ok and rev or nil
end

return { init = init, func = filter }
