OBJECTS = $(addprefix bin/, \
	lex.o )

CFLAGS = -g -O0 -Wall -Iinclude

all: bin bin/dependent-c

bin/dependent-c: bin/main.o $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: bin

bin:
	mkdir -p $@

.PHONY: clean

clean:
	rm -r bin

test: bin/test bin/test-dependent-c
	./bin/test-dependent-c

bin/test-dependent-c: bin/test/main.o $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

bin/test/%.o: test/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: bin/test

bin/test:
	mkdir -p $@
