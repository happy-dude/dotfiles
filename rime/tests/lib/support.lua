-- Stand-ins for the parts of librime-lua the filters use, backed by the
-- dictionary rows in rime/tests/fixtures. Run the tests from the repository
-- root.

local M = {}

local fixtures = 'rime/tests/fixtures/'

package.path = 'rime/.local/share/fcitx5/rime/lua/?.lua;' .. package.path

-- What rime/reading_tables.py writes from the fixture dictionaries; the
-- rime-reading-tables check holds the file to that. It is not named .lua so
-- that the formatter leaves the generator's layout alone.
package.loaded['reading_tables'] = dofile(fixtures .. 'reading_tables.generated')

-- A ReverseLookup over fixture dictionaries. It answers as librime's reverse
-- database does: a character's codes in code order, and an empty string for
-- a phrase or an unknown character.
local function reverse_lookup(files)
  local codes = {}
  for _, file in ipairs(files) do
    local body = false
    for line in io.lines(fixtures .. file) do
      if not body then
        body = line == '...'
      else
        local text, code = line:match('^([^\t#]+)\t([^\t ]+)')
        local phrase = line:match('^[^\t]+\t[^\t]* ')
        if text and not phrase then
          codes[text] = codes[text] or {}
          codes[text][code] = true
        end
      end
    end
  end
  return {
    lookup = function(_, text)
      local sorted = {}
      for code in pairs(codes[text] or {}) do
        sorted[#sorted + 1] = code
      end
      table.sort(sorted)
      return table.concat(sorted, ' ')
    end,
  }
end

local dictionaries = {
  terra_pinyin = reverse_lookup({ 'terra_pinyin.dict.yaml' }),
  luna_pinyin = reverse_lookup({ 'luna_pinyin.dict.yaml' }),
  jyut6ping3 = reverse_lookup({ 'jyut6ping3.chars.dict.yaml' }),
}

function ReverseLookup(name)
  return dictionaries[name] or error('no dictionary named ' .. name)
end

M.ReverseLookup = ReverseLookup

-- Run a filter over candidates ({ text =, preedit =, comment = }) and return
-- the comment each one was yielded with.
function M.comments(filter, candidates)
  local env = {}
  filter.init(env)
  local yielded = {}
  yield = function(candidate)
    yielded[#yielded + 1] = candidate.comment or ''
  end
  local input = {
    iter = function()
      local position = 0
      return function()
        position = position + 1
        return candidates[position]
      end
    end,
  }
  filter.func(input, env)
  yield = nil
  assert(#yielded == #candidates, 'the filter must yield every candidate once')
  return yielded
end

function M.equal(actual, expected, label)
  assert(
    actual == expected,
    string.format('%s: expected %s, got %s', label, tostring(expected), tostring(actual))
  )
end

return M
