module Main where
	main :: Char -> Char
	main x
		|mod x 2 == 0 = x*x
		|mod x 2 == 1 = log x