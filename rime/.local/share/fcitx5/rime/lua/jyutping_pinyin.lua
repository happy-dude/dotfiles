-- jyutping_pinyin.lua
-- Adds tonal pinyin annotations to jyutping candidates

local function convert_tones(pinyin)
  if not pinyin or pinyin == "" then return nil end

  pinyin = pinyin:gsub("([aeiouv])(ng?)([1-4])", "%1%3%2")
  pinyin = pinyin:gsub("([aeo])([iuo])([1-4])", "%1%3%2")

  local marks = {
    a = {"ā", "á", "ǎ", "à"},
    e = {"ē", "é", "ě", "è"},
    i = {"ī", "í", "ǐ", "ì"},
    o = {"ō", "ó", "ǒ", "ò"},
    u = {"ū", "ú", "ǔ", "ù"},
    v = {"ǖ", "ǘ", "ǚ", "ǜ"},
  }

  for vowel, tones in pairs(marks) do
    for i = 1, 4 do
      pinyin = pinyin:gsub(vowel .. i, tones[i])
    end
  end

  pinyin = pinyin:gsub("5", "")
  pinyin = pinyin:gsub("v", "ü")

  return pinyin
end

local function filter(input, env)
  for cand in input:iter() do
    local text = cand.text

    if text and text ~= "" and env.terra then
      local parts = {}

      for p, c in utf8.codes(text) do
        local char = utf8.char(c)
        local ok, code = pcall(function() return env.terra:lookup(char) end)
        if ok and code and code ~= "" then
          local first = code:match("^([^\t ]+)")
          if first then
            local converted = convert_tones(first)
            if converted then
              table.insert(parts, converted)
            end
          end
        end
      end

      if #parts > 0 then
        -- Append pinyin to existing jyutping comment
        local existing = cand.comment or ""
        if existing ~= "" then
          cand.comment = existing .. " [" .. table.concat(parts, " ") .. "]"
        else
          cand.comment = table.concat(parts, " ")
        end
      end
    end

    yield(cand)
  end
end

local function init(env)
  local ok, rev = pcall(ReverseLookup, "terra_pinyin")
  env.terra = ok and rev or nil
end

return { init = init, func = filter }
