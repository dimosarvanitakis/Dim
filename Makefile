# Declaration of variables
CC  = clang
CPP = clang++

CFLAGS   = -std=c11 -Wall -Wextra
DEBUG    = -ggdb -DDEBUG
CLLVM    = `llvm-config --cflags`
CPPLLVM  = `llvm-config --cxxflags --ldflags --libs core executionengine interpreter analysis native bitwriter --system-libs`
DEPFLAGS = -MMD -MF $(@:.o=.d)

# File names
EXEC   := dim
SOURCES = $(wildcard src/*.c)
HEADERS = $(wildcard src/*.h)
OBJECTS = ${SOURCES:src/%.c=obj/%.o}
DEPS    = $(patsubst %.o,%.d,$(OBJECTS))

default: $(EXEC)

obj/%.o: src/%.c $(HEADERS)
	@mkdir -p $(dir $@)
	$(CC) $(CLLVM) $(DEBUG) $(CFLAGS) -c $< $(DEPFLAGS) -o $@

$(EXEC): $(OBJECTS)
	$(CPP) $(CPPLLVM) -o $@ $^

clean:
	rm -rf obj deps
	rm -rf $(EXEC)
