-- test.lua
-- Simple test filter that adds "TEST" to all candidate comments

local function filter(input, env)
  for cand in input:iter() do
    cand.comment = "TEST " .. (cand.comment or "")
    yield(cand)
  end
end

return { func = filter }

