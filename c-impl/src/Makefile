main: main.o cipher.o utils.o
	gcc -o $@ $^
cipher.o: cipher.c cipher.h
	gcc -c $<
utils.o : utils.c utils.h
	gcc -c $<