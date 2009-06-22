#!/usr/local/bin/lua-5.1

my={}
my.read = function (str)
  local function numberp (char)
    tmp = string.byte(char)
    if tmp ~= nil and tmp > 47 and tmp < 58 then
      return true
    else
      return false
    end
  end
  for w=1,string.len(str) do
    if numberp(string.sub(str,w,w+1)) then
      buf={}
      while numberp(string.sub(str,w,w+1)) do
        table.insert(buf, string.sub(str,w,w+1))
        w = w + 1
      end
      print(w)
    end
  end
end

my.read("109")
