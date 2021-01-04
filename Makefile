tinker:
	cat tinker.txt | stack ghci

format_all:
	hindent src/**/*.hs
	stylish-haskell -i src/**/*.hs
