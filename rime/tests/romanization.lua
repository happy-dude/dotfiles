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
  { 'ma', 'ma' },
  -- The mark goes on a or e, on the o of ou, and otherwise on the last vowel.
  { 'guo2', 'guó' },
  { 'lve4', 'lüè' },
  { 'nve4', 'nüè' },
  { 'kuai4', 'kuài' },
  { 'xiong2', 'xióng' },
  { 'dou4', 'dòu' },
  { 'yue1', 'yuē' },
  -- ê is spelled eh.
  { 'eh1', 'ê̄' },
  { 'eh2', 'ế' },
  { 'eh5', 'ê' },
  -- A syllabic nasal has no vowel and carries the mark itself.
  { 'ng2', 'ńg' },
  { 'm2', 'ḿ' },
  { 'n4', 'ǹ' },
  { 'hng5', 'hng' },
  { 'hm1', 'hm̄' },
  -- Nothing to mark: the tone is kept as a digit, not dropped.
  { 'r2', 'r2' },
  { 'r5', 'r' },
  { 'zhong4 yao4', 'zhòng yào' },
}

for _, case in ipairs(cases) do
  -- Callers pass the result straight into table.insert, which rejects a
  -- third argument, so the converter must return exactly one value.
  local count = select('#', romanization.convert_tones(case[1]))
  assert(count == 1, string.format('%s: expected 1 result, got %d', case[1], count))
  local actual = romanization.convert_tones(case[1])
  assert(actual == case[2], string.format('%s: expected %s, got %s', case[1], case[2], actual))
end
