package.path = 'rime/tests/lib/?.lua;' .. package.path

local support = require('support')
local tonal_pinyin = require('tonal_pinyin')
local jyutping_pinyin = require('jyutping_pinyin')

-- Each case is { text, preedit, comment before the filter, comment after }.
local function check(filter, cases)
  local candidates = {}
  for position, case in ipairs(cases) do
    candidates[position] = { text = case[1], preedit = case[2], comment = case[3] }
  end
  local comments = support.comments(filter, candidates)
  for position, case in ipairs(cases) do
    support.equal(comments[position], case[4], case[1] .. ' typed as ' .. tostring(case[2]))
  end
end

-- The Pinyin schema: Pinyin is typed, the Jyutping in brackets is not.
check(tonal_pinyin, {
  { '長大', 'zhang da', '', 'zhǎng dà [zoeng2 daai6]' },
  { '音樂', 'yin yue', '', 'yīn yuè [jam1 ngok6]' },
  { '重要', 'zhong yao', '', 'zhòng yào [zung6 jiu3]' },
  { '銀行', 'yin hang', '', 'yín háng [ngan4 hong4]' },
  { '行路', 'xing lu', '', 'xíng lù [haang4 lou6]' },
  { '快樂', 'kuai le', '', 'kuài lè [faai3 lok6]' },
  { '快了', 'kuai le', '', 'kuài le [faai3 liu5]' },
  { '行', 'hang', '', 'háng [haang4]' },
  { '行', 'xing', '', 'xíng [haang4]' },
  -- An abbreviation leaves the choice to the dictionaries.
  { '長大', 'z d', '', 'zhǎng dà [zoeng2 daai6]' },
  -- terra_pinyin reads 撂 only as liao4; luna_pinyin also has lve, which
  -- was typed, so it is shown, without a tone because neither lists one.
  { '撂', 'lüe', '', 'lüe [liu1]' },
  { '撂', 'liao', '', 'liào [liu1]' },
  -- A character with no reading keeps its place in both halves.
  { '長a', nil, '', 'cháng ? [zoeng2 ?]' },
  -- Nothing to show leaves the comment alone.
  { 'ab', 'ab', '', '' },
  { '，', nil, 'kept', 'kept' },
  -- An existing comment stays in front.
  { '重要', 'zhong yao', '~', '~ zhòng yào [zung6 jiu3]' },
})

-- The Cantonese schema: the Jyutping comment is the stock schema's, and the
-- Pinyin in brackets was not typed.
check(jyutping_pinyin, {
  { '重要', 'zung jiu', 'zung6 jiu3', 'zung6 jiu3 [zhòng yào]' },
  { '音樂', 'jam ngok', 'jam1 ngok6', 'jam1 ngok6 [yīn yuè]' },
  { '長大', 'zoeng daai', 'zoeng2 daai6', 'zoeng2 daai6 [zhǎng dà]' },
  { '快樂', 'faai lok', 'faai3 lok6', 'faai3 lok6 [kuài lè]' },
  { '銀行', 'ngan hong', 'ngan4 hong4', 'ngan4 hong4 [yín háng]' },
  { '行路', 'haang lou', 'haang4 lou6', 'haang4 lou6 [xíng lù]' },
  { '仲要', 'zung jiu', 'zung6 jiu3', 'zung6 jiu3 [zhòng yào]' },
  { '長', 'coeng', 'coeng4', 'coeng4 [cháng]' },
  { '長a', nil, '', 'cháng ?' },
  { 'ab', nil, 'kept', 'kept' },
})
