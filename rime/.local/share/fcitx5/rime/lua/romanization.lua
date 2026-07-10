local M = {}

local tone_marks = {
  a = { 'ā', 'á', 'ǎ', 'à' },
  e = { 'ē', 'é', 'ě', 'è' },
  i = { 'ī', 'í', 'ǐ', 'ì' },
  o = { 'ō', 'ó', 'ǒ', 'ò' },
  u = { 'ū', 'ú', 'ǔ', 'ù' },
  v = { 'ǖ', 'ǘ', 'ǚ', 'ǜ' },
}

function M.convert_tones(pinyin)
  if not pinyin or pinyin == '' then
    return nil
  end

  pinyin = pinyin:gsub('([aeiouv])(ng?)([1-4])', '%1%3%2')
  pinyin = pinyin:gsub('([aeo])([iuo])([1-4])', '%1%3%2')

  for vowel, tones in pairs(tone_marks) do
    for i = 1, 4 do
      pinyin = pinyin:gsub(vowel .. i, tones[i])
    end
  end

  pinyin = pinyin:gsub('5', '')
  return pinyin:gsub('v', 'ü')
end

return M
