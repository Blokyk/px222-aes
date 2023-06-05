main: main.o cipher.o utils.o
	gcc -o $@ $^
cipher.o: cipher.c cipher.h 
	gcc -c $<
utils.o : Utils.c Utils.h
	gcc -c $<