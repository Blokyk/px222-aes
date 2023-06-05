main: main.o cipher.o
	gcc -o $@ $^
cipher.o: cipher.c cipher.h
	gcc -c $<