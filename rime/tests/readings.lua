package.path = 'rime/tests/lib/?.lua;' .. package.path

local support = require('support')
local readings = require('readings')

local equal = support.equal

local function joined(list)
  return table.concat(list, ' ')
end

-- Characters of one to four bytes, in order.
equal(joined(readings.chars('長a，𠮷')), '長 a ， 𠮷', 'chars')
equal(#readings.chars(''), 0, 'chars of an empty string')

equal(readings.toneless('zhang3'), 'zhang', 'toneless')
equal(readings.toneless('lve'), 'lve', 'toneless without a tone')

equal(joined(readings.typed_pinyin('zhang da', 2)), 'zhang da', 'typed syllables')
equal(joined(readings.typed_pinyin("xi'an", 2)), 'xi an', 'typed delimiter')
equal(joined(readings.typed_pinyin('lüe', 1)), 'lve', 'typed ü')
equal(readings.typed_pinyin('zhang da', 3), nil, 'syllables that do not align')
equal(readings.typed_pinyin(nil, 1), nil, 'no preedit')

local tables = readings.tables()
local terra = support.ReverseLookup('terra_pinyin')
local pinyin = readings.resolver(terra, tables.pinyin)
local jyutping = readings.resolver(support.ReverseLookup('jyut6ping3'), tables.jyutping)

local function resolved(resolver, text, typed)
  local chars = readings.chars(text)
  return readings.join(resolver:resolve(chars, typed), #chars)
end

-- The reverse dictionary alone gives the alphabetically first reading; this
-- is what the tables exist to correct.
local unranked = readings.resolver(terra, nil)
equal(resolved(unranked, '重要'), 'chong2 yao1', 'reverse dictionary order')
equal(resolved(unranked, '行'), 'hang2', 'reverse dictionary order, one character')

-- By dictionary weight.
equal(resolved(pinyin, '重'), 'zhong4', 'weight')
equal(resolved(pinyin, '行'), 'xing2', 'weight')
equal(resolved(pinyin, '要'), 'yao4', 'weight')
equal(resolved(pinyin, '重要'), 'zhong4 yao4', 'weight, two characters')
equal(resolved(jyutping, '樂'), 'lok6', 'weight')
equal(resolved(jyutping, '了'), 'liu5', 'weight')

-- By phrase, where the commonest readings of the characters are wrong.
equal(resolved(pinyin, '音樂'), 'yin1 yue4', 'phrase')
equal(resolved(pinyin, '銀行'), 'yin2 hang2', 'phrase')
equal(resolved(pinyin, '長大'), 'zhang3 da4', 'phrase')
equal(resolved(pinyin, '快樂'), 'kuai4 le4', 'phrase the weights get right')
equal(resolved(jyutping, '銀行'), 'ngan4 hong4', 'phrase')
equal(resolved(jyutping, '音樂'), 'jam1 ngok6', 'phrase')
equal(resolved(jyutping, '重要'), 'zung6 jiu3', 'phrase')

-- Phrases are found inside a longer candidate, from the left.
equal(resolved(pinyin, '行銀行'), 'xing2 yin2 hang2', 'phrase after a character')
equal(resolved(pinyin, '銀行行'), 'yin2 hang2 xing2', 'phrase before a character')
equal(resolved(pinyin, '音樂銀行'), 'yin1 yue4 yin2 hang2', 'two phrases')

-- What was typed rules out the other readings, and overrides a phrase.
equal(resolved(pinyin, '長大', { 'zhang', 'da' }), 'zhang3 da4', 'typed')
equal(resolved(pinyin, '長大', { 'chang', 'da' }), 'chang2 da4', 'typed against the phrase')
equal(resolved(pinyin, '行', { 'hang' }), 'hang2', 'typed against the weight')
equal(resolved(pinyin, '重要', { 'zhong', 'yao' }), 'zhong4 yao4', 'typed, tone by weight')
equal(resolved(pinyin, '音樂', { 'yin', 'le' }), 'yin1 le4', 'typed against the phrase')

-- A typed syllable that spells no reading of the character says nothing.
equal(resolved(pinyin, '長大', { 'z', 'd' }), 'zhang3 da4', 'abbreviation')
equal(resolved(pinyin, '行', { 'xnig' }), 'xing2', 'typo')

-- A character without a reading keeps its place.
equal(resolved(pinyin, '長a大'), 'chang2 ? da4', 'placeholder')
equal(resolved(pinyin, 'ab'), nil, 'nothing to show')
equal(resolved(readings.resolver(nil, nil), '長'), nil, 'no dictionary')
