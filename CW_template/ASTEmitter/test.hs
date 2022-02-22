module Main where
x = 3
f = \y -> x+y
x = 5
main = print $ f 10

