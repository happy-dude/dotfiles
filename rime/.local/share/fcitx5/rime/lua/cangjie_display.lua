-- cangjie_display.lua
-- Displays Cangjie codes: | NLDW 陳 弓中木田 | AGGU 曉 日土土山 |

local readings = require('readings')

-- Cangjie letter → radical
--  Q  W  E  R  T  Y  U  I  O  P
--  手 田 水 口 廿 卜 山 戈 人 心
--
--  A  S  D  F  G  H  J  K  L
--  日 尸 木 火 土 竹 十 大 中
--
--  Z  X  C  V  B  N  M
--  符 難 金 女 月 弓 一
--
local cangjie_radicals = {
  q = '手',
  w = '田',
  e = '水',
  r = '口',
  t = '廿',
  y = '卜',
  u = '山',
  i = '戈',
  o = '人',
  p = '心',
  a = '日',
  s = '尸',
  d = '木',
  f = '火',
  g = '土',
  h = '竹',
  j = '十',
  k = '大',
  l = '中',
  z = '符',
  x = '難',
  c = '金',
  v = '女',
  b = '月',
  n = '弓',
  m = '一',
}

local function code_to_radicals(code)
  if not code or code == '' then
    return nil
  end
  local radicals = {}
  for i = 1, #code do
    local letter = code:sub(i, i):lower()
    local radical = cangjie_radicals[letter]
    if radical then
      table.insert(radicals, radical)
    end
  end
  if #radicals > 0 then
    return table.concat(radicals, '')
  end
  return nil
end

local function filter(input, env)
  for cand in input:iter() do
    local text = cand.text

    if text and text ~= '' then
      local parts = {}

      for _, char in ipairs(readings.chars(text)) do
        -- A character's codes are alternative spellings of the same shape,
        -- not readings to choose between; the first one is shown.
        local code = readings.codes(env.cangjie, char)[1]

        if code then
          local upper_code = code:upper()
          local radicals = code_to_radicals(code)

          if radicals then
            table.insert(parts, upper_code .. ' ' .. char .. ' ' .. radicals)
          else
            table.insert(parts, upper_code .. ' ' .. char)
          end
        end
      end

      if #parts > 0 then
        cand.comment = '| ' .. table.concat(parts, ' | ') .. ' |'
      end
    end

    yield(cand)
  end
end

local function init(env)
  env.cangjie = nil

  local ok, rev = pcall(ReverseLookup, 'cangjie5')
  if ok and rev then
    env.cangjie = rev
  end
end

return { init = init, func = filter }
