package.path = 'rime/.local/share/fcitx5/rime/lua/?.lua;' .. package.path

local romanization = require('romanization')

local cases = {
  { 'ma1', 'mā' },
  { 'jing3', 'jǐng' },
  { 'er2', 'ér' },
  { 'xiao3', 'xiǎo' },
  { 'gui4', 'guì' },
  { 'liu2', 'liú' },
  { 'nv3', 'nǚ' },
  { 'ma5', 'ma' },
}

for _, case in ipairs(cases) do
  -- Callers pass the result straight into table.insert, which rejects a
  -- third argument, so the converter must return exactly one value.
  local count = select('#', romanization.convert_tones(case[1]))
  assert(count == 1, string.format('%s: expected 1 result, got %d', case[1], count))
  local actual = romanization.convert_tones(case[1])
  assert(actual == case[2], string.format('%s: expected %s, got %s', case[1], case[2], actual))
end
