-- tonal_pinyin.lua

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

    if text and text ~= "" then
      local parts = {}

      for p, c in utf8.codes(text) do
        local char = utf8.char(c)
        local py = nil

        -- Try terra_pinyin first (has tones)
        if env.terra then
          local ok, code = pcall(function() return env.terra:lookup(char) end)
          if ok and code and code ~= "" then
            local first = code:match("^([^\t ]+)")
            if first then
              py = convert_tones(first)
            end
          end
        end

        -- Fallback to luna_pinyin (no tones)
        if not py and env.luna then
          local ok, code = pcall(function() return env.luna:lookup(char) end)
          if ok and code and code ~= "" then
            local first = code:match("^([^\t ]+)")
            if first then
              py = first
            end
          end
        end

        if py then
          table.insert(parts, py)
        end
      end

      if #parts > 0 then
        cand.comment = table.concat(parts, " ")
      end
    end

    yield(cand)
  end
end

local function init(env)
  env.terra = nil
  env.luna = nil

  local ok1, rev1 = pcall(ReverseLookup, "terra_pinyin")
  if ok1 and rev1 then env.terra = rev1 end

  local ok2, rev2 = pcall(ReverseLookup, "luna_pinyin")
  if ok2 and rev2 then env.luna = rev2 end
end

return { init = init, func = filter }
