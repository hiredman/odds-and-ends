#!/usr/local/bin/ruby -Ku
module Kernel; alias λ proc; end

empty_env = λ{Array.new}

extend_env = λ{|name, value, environment| environment.push [name, value]}

apply_env = λ{|environment, name|
  r=:UNDEFINED
  environment.each {|i| r=i[1] unless i[0] != name}
  r}

whitespace = λ{|char| " \n\t"[char]}

expression = λ{|string, n, o|
  diff_exp = λ{|string, n, o|
    buf=nil
    if string[n].chr == "-" then
      n += 2
      p = 1
      buf=""
      left = nil
      while p != 0 do
        if string[n].chr == "(" then p += 1 end
        if string[n].chr == ")" then p -= 1 end
        buf+=string[n].chr
        if string[n].chr == "," and p == 1 then
          left = buf
        end
        n += 1
      end
      left=left.slice(0..-2)
      right=buf.slice(left.length+1..-2)
      x=Array.new
      x.push :"-"
      left=expression.call(left, 0, x)
      right=expression.call(right, 0, x)
      buf=x
    end
    o.push buf unless buf == nil
    n
  }
  symbol = λ{|string, n, o|
    buf=""
    while n < string.length and !whitespace.call string[n].chr do
      buf += string[n].chr
      n += 1
    end
    o.push buf.to_sym unless buf == ""
    n
  }
  number = λ{|string, n, o|
    buf=""
    while n < string.length and !whitespace.call string[n].chr and string[n].chr[/[0-9]/] != nil do
      buf += string[n].chr
      n += 1
    end
    o.push buf.to_i unless buf == ""
    n
  }
  while n < string.length do
    n = diff_exp.call(string, n, o)
    n = number.call(string, n, o)
    n = symbol.call(string, n, o)
    n += 1
  end
  o
}

value_of = λ{|ast, environment|
  if ast.is_a? Array then
    case ast[0]
      when :let then
        value_of.call(ast[5], extend_env.call(ast[1], value_of.call(ast[3],environment), environment))
      when :- then
        value_of.call(ast[1],environment) - value_of.call(ast[2],environment)
    end
  else
    if ast.is_a? Fixnum then
      ast
    elsif ast.is_a? Symbol then
      apply_env.call(environment, ast)
    end
  end
}

run = λ{|string| value_of.call(expression.call(string,0,Array.new), empty_env.call)}

p run.call("let x = -(5, 2) in -(x, 1)")
