heron a b c = result
	where 
	result = sqrt(s * (s - a) * (s - b) * (s - c))
	s      = (a + b + c) / 2

main = heron 1 2 3
