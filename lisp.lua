#!/usr/local/bin/lua-5.1

List = {}
local list_meta = {}
function List.cons (a, b)
  return {car=a,cdr=b}
end
function List.new (...)
  l = arg
  l.c = 1
  li = List.list(l)
  setmetatable(li,list_meta)
  return li
end
function List.list(l)
  if l.c <= l.n then
    a = l[l.c]
    l.c = l.c + 1
    return List.cons(a, List.list(l))
  else
    return nil
  end
end
function List.tostring(lst)
  p = lst
  buf=""
  while p ~= nil do
    buf = buf .. to_s(p.car) 
    if p.cdr ~= nil then
      buf = buf .. " "
    end
    p = p.cdr
  end
  return "(".. buf ..")"
end
function List.eval(a, b)
  return Interpreter.apply(Interpreter.eval(List.first(a)),List.map(function (a) return Interpreter.eval(a, b); end,List.rest(a)))
end
function List.first(a)
  return a.car
end
function List.rest(a)
  p = a.cdr
  if p ~= nil then
    setmetatable(p, list_meta)
  end
  return p
end
function List.map(f, lst)
  if lst ~= nil then
    return List.cons(f(List.first(lst)), List.map(f, List.rest(lst)))
  else
    return lst
  end
end
list_meta.__tostring = List.tostring
list_meta.__index = List

Symbol = {}
local symbol_meta = {}
function Symbol.new (s)
  m = {name=s}
  setmetatable(m,symbol_meta)
  return m
end
function Symbol.tostring (s)
  return s.name
end
function Symbol.eval (s)
  return {primitive = true, name=s.name}
end
symbol_meta.__tostring = Symbol.tostring
symbol_meta.__index = Symbol 

Interpreter = {}
function Interpreter.to_s(thing) 
  if thing == nil then
    return "nil"
  elseif type(thing) == "number" then
    return ""..thing
  else
    return thing:tostring()
  end
end
function Interpreter.eval(thing, env)
  if thing == nil then
    return nil
  elseif type(thing) == "number" then
    return thing
  else
    return thing:eval(env)
  end
end
function Interpreter.apply(fn, args)
  if fn.primitive ~= nil then
    return Interpreter.apply_prim(fn.name, args)
  end
end
function Interpreter.apply_prim(fn, args)
  if fn == "+" then
    p = args
    ac = 0
    while p ~= nil do
      ac = ac + List.first(p)
      p = List.rest(p)
    end
    return ac
  end
end

l1 = List.new(Symbol.new("+"), 2, 3)
l2 = List.new(4, 5, 6)
n = Interpreter.eval(l1, nil)
print(n)
