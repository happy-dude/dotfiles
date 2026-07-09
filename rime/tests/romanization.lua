package.path = "rime/.local/share/fcitx5/rime/lua/?.lua;" .. package.path

local romanization = require("romanization")

local cases = {
  { "ma1", "mā" },
  { "jing3", "jǐng" },
  { "xiao3", "xiǎo" },
  { "gui4", "guì" },
  { "liu2", "liú" },
  { "nv3", "nǚ" },
  { "ma5", "ma" },
}

for _, case in ipairs(cases) do
  local actual = romanization.convert_tones(case[1])
  assert(actual == case[2], string.format("%s: expected %s, got %s", case[1], case[2], actual))
end
