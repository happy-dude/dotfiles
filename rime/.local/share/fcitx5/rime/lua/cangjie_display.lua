-- cangjie_display.lua
-- Displays Cangjie codes: | NLDW 陳 弓中田木 | AGGU 曉 日土土山 |

local cangjie_radicals = {
  a = "日", b = "月", c = "金", d = "木", e = "水", f = "火", g = "土",
  h = "竹", i = "戈", j = "十", k = "大", l = "中", m = "一", n = "弓",
  o = "人", p = "心", q = "手", r = "口", s = "尸", t = "廿", u = "山",
  v = "女", w = "田", x = "難", y = "卜", z = "符"
}

local function code_to_radicals(code)
  if not code or code == "" then return nil end
  local radicals = {}
  for i = 1, #code do
    local letter = code:sub(i, i):lower()
    local radical = cangjie_radicals[letter]
    if radical then
      table.insert(radicals, radical)
    end
  end
  if #radicals > 0 then
    return table.concat(radicals, "")
  end
  return nil
end

local function get_cangjie(env, char)
  if env.cangjie then
    local ok, code = pcall(function() return env.cangjie:lookup(char) end)
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
      local parts = {}

      for p, c in utf8.codes(text) do
        local char = utf8.char(c)
        local code = get_cangjie(env, char)

        if code then
          local upper_code = code:upper()
          local radicals = code_to_radicals(code)

          if radicals then
            -- Format: NLDW 陳 弓中田木
            table.insert(parts, upper_code .. " " .. char .. " " .. radicals)
          else
            table.insert(parts, upper_code .. " " .. char)
          end
        end
      end

      if #parts > 0 then
        -- Format: | NLDW 陳 弓中田木 | AGGU 曉 日土土山 |
        cand.comment = "| " .. table.concat(parts, " | ") .. " |"
      end
    end

    yield(cand)
  end
end

local function init(env)
  env.cangjie = nil

  local ok, rev = pcall(ReverseLookup, "cangjie5")
  if ok and rev then env.cangjie = rev end
end

return { init = init, func = filter }
