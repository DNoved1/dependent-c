#=== Shared Definitions =======================================================
OBJECTS = $(addprefix bin/, \
	general.o \
	lex.o grammar/dependent-c.y.o \
	ast.o symbol_table.o type.o )

CFLAGS = -g -O0 -std=c99 -pedantic -Wall -Werror -Iinclude
BISONFLAGS = -Wall -Werror

#=== Building the Compiler ====================================================
all: bin bin/grammar bin/dependent-c

bin/dependent-c: bin/main.o $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: bin
bin:
	mkdir -p $@

bin/grammar/%.o: bin/grammar/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

bin/grammar/dependent-c.y.c: grammar/dependent-c.y
	bison $(BISONFLAGS) -o $@ $^

.PHONY: bin/grammar
bin/grammar:
	mkdir -p $@

#=== Cleaning =================================================================
.PHONY: clean
clean:
	rm -rf bin

#=== Testing ==================================================================
TEST_OBJECTS = $(addprefix bin/test/, \
	lex.o )

test: bin/test bin/test-dependent-c
	./bin/test-dependent-c

bin/test-dependent-c: bin/test/main.o $(TEST_OBJECTS) $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

bin/test/%.o: test/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: bin/test

bin/test:
	mkdir -p $@
