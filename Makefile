OBJECTS = $(addprefix bin/, \
	main.o lex.o )

CFLAGS = -g -O0 -Wall -Iinclude

all: bin bin/dependent-c

bin/dependent-c: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: bin

bin:
	mkdir -p $@

.PHONY: clean

clean:
	rm -r bin
