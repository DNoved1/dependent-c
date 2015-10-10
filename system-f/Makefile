OBJECTS = $(addprefix bin/, \
	util.o ast.o context.o parse.o \
	)

CXXFLAGS = -g -O0 -std=c++14 -pedantic -Wall -Werror -Iinclude

all: bin bin/system-f-c

#=== Building the Compiler ====================================================
bin/system-f-c: bin/main.o $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^

bin/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

.PHONY: bin
bin:
	mkdir -p $@

#=== Cleaning =================================================================
.PHONY: clean
clean:
	rm -rf bin
