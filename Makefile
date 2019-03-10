##
## EPITECH PROJECT, 2019
## KOAK
## File description:
## Makefile
##

all: koak

koak:
	stack build
	echo '#!/usr/bin/bash' >  koak
	echo 'stack exec koak-exe $$1 > out.ll' >> koak
	echo 'clang out.ll' >> koak
	echo './a.out' >> koak
	echo 'rm -f out.ll' >> koak
	chmod a+x koak

clean:
	stack clean

fclean: clean
	rm -f koak

re: fclean all

.PHONY: all clean fclean re
