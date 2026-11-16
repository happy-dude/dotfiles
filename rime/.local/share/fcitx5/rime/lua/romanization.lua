local M = {}

-- Each letter that can carry a tone mark, with tones 1 to 4. The marks on ê
-- (tones 1 and 3), on m (1, 3, and 4), and on n (1) have no precomposed
-- character and are the letter followed by a combining mark.
local tone_marks = {
  a = { 'ā', 'á', 'ǎ', 'à' },
  e = { 'ē', 'é', 'ě', 'è' },
  i = { 'ī', 'í', 'ǐ', 'ì' },
  o = { 'ō', 'ó', 'ǒ', 'ò' },
  u = { 'ū', 'ú', 'ǔ', 'ù' },
  v = { 'ǖ', 'ǘ', 'ǚ', 'ǜ' },
  ['ê'] = { 'ê̄', 'ế', 'ê̌', 'ề' },
  m = { 'm̄', 'ḿ', 'm̌', 'm̀' },
  n = { 'n̄', 'ń', 'ň', 'ǹ' },
}

-- Where the mark goes: on a or e when the syllable has one, on the o of ou,
-- and otherwise on the last vowel. A syllable without a vowel is a syllabic
-- nasal (m, n, ng, hm, hng) and carries the mark on the nasal.
local function marked_letter(letters)
  local position = letters:find('[ae]')
    or letters:find('ou')
    or letters:find('[iouv][^iouv]*$')
    or letters:find('[mn]')
  return position
end

local function convert_syllable(letters, tone)
  -- terra_pinyin spells the vowel ê, as in 欸, as eh.
  local ehat = letters == 'eh'
  local position = not ehat and marked_letter(letters)
  local marks = tone_marks[ehat and 'ê' or position and letters:sub(position, position)]
  tone = tonumber(tone)

  local marked
  if ehat then
    marked = tone and marks[tone] or 'ê'
  elseif tone and marks and marks[tone] then
    marked = letters:sub(1, position - 1) .. marks[tone] .. letters:sub(position + 1)
  elseif tone and tone < 5 then
    -- Nothing can carry the mark; keep the digit instead of dropping the tone.
    marked = letters .. tone
  else
    marked = letters
  end
  return (marked:gsub('v', 'ü'))
end

-- "zhang3" to "zhǎng"; tone 5 and a missing tone are the unmarked neutral
-- tone. Returns exactly one value.
function M.convert_tones(pinyin)
  if not pinyin or pinyin == '' then
    return nil
  end
  return (pinyin:gsub('(%a+)([1-5]?)', convert_syllable))
end

return M
