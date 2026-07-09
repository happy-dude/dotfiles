-- tonal_pinyin.lua (WITH JYUTPING)

local romanization = require("romanization")

local function get_pinyin(env, char)
  -- Try terra_pinyin first (has tones)
  if env.terra then
    local ok, code = pcall(function() return env.terra:lookup(char) end)
    if ok and code and code ~= "" then
      local first = code:match("^([^\t ]+)")
      if first then
        return romanization.convert_tones(first)
      end
    end
  end

  -- Fallback to luna_pinyin (no tones)
  if env.luna then
    local ok, code = pcall(function() return env.luna:lookup(char) end)
    if ok and code and code ~= "" then
      local first = code:match("^([^\t ]+)")
      if first then
        return first
      end
    end
  end

  return nil
end

local function get_jyutping(env, char)
  if env.jyut then
    local ok, code = pcall(function() return env.jyut:lookup(char) end)
    if ok and code and code ~= "" then
      local first = code:match("^([^\t ]+)")
      if first then
        return first
      end
    end
  end
  return nil
end

local function filter(input, env)
  for cand in input:iter() do
    local text = cand.text

    if text and text ~= "" then
      local pinyin_parts = {}
      local jyutping_parts = {}

      for p, c in utf8.codes(text) do
        local char = utf8.char(c)

        local py = get_pinyin(env, char)
        if py then
          table.insert(pinyin_parts, py)
        end

        local jp = get_jyutping(env, char)
        if jp then
          table.insert(jyutping_parts, jp)
        end
      end

      -- Build comment with both pinyin and jyutping
      local comment_parts = {}

      if #pinyin_parts > 0 then
        table.insert(comment_parts, table.concat(pinyin_parts, " "))
      end

      if #jyutping_parts > 0 then
        table.insert(comment_parts, "[" .. table.concat(jyutping_parts, " ") .. "]")
      end

      if #comment_parts > 0 then
        cand.comment = table.concat(comment_parts, " ")
      end
    end

    yield(cand)
  end
end

local function init(env)
  env.terra = nil
  env.luna = nil
  env.jyut = nil

  local ok1, rev1 = pcall(ReverseLookup, "terra_pinyin")
  if ok1 and rev1 then env.terra = rev1 end

  local ok2, rev2 = pcall(ReverseLookup, "luna_pinyin")
  if ok2 and rev2 then env.luna = rev2 end

  local ok3, rev3 = pcall(ReverseLookup, "jyut6ping3")
  if ok3 and rev3 then env.jyut = rev3 end
end

return { init = init, func = filter }
