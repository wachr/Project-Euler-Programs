#! /usr/bin/ruby

# euler-prob-04.rb
# author: Ray Wach
# date: 2014-10-02
# info: Program to solve Project Euler problem 4 in Ruby.

def palindrome?(arg)
  if (arg.to_s.reverse == arg.to_s)
    true
  else
    false
  end
end

num_digits = ARGV[0].to_i

if (ARGV[0].nil?)
  puts "Usage: #{$0} <num_digits>"
  exit(1)
end

ceiling = ('9' * num_digits).to_i
floor = ('1' + '0' * (num_digits - 1)).to_i

puts "Searching for the largest palindrome product in [#{floor},#{ceiling}]."

left = right = ceiling
max_pal = 0
max_pal = left * right if (palindrome?(left * right))

until (left < floor) do
  if (max_pal < left * right) && (palindrome?(left * right))
    max_pal = left * right
  end
  right -= 1
  if (right < floor)
    left -= 1
    right = ceiling
  end
end

puts "#{max_pal}"
