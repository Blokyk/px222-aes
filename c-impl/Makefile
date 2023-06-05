
main: src/main.o src/cipher.o src/utils.o
	gcc -o $@ $^

test: main tests/test.o src/cipher.o src/utils.o
	gcc -o $@ $^

clean:
	rm -r *.o main