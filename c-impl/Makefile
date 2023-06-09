CC ?= gcc
WARN += all extra
NO_WARN += unused-value format-extra-args
CFLAGS += $(addprefix -W, $(WARN)) $(addprefix -Wno-, $(NO_WARN))
CFLAGS += -march=native -g -flto -fshort-enums -fsanitize=address,leak
LDFLAGS += -flto -fsanitize=address,leak

OBJ_DIR = obj

BIN_DIR = bin

SRC := $(wildcard src/*.c)
OBJS = $(addprefix $(OBJ_DIR)/, $(SRC:.c=.o))
DEPS = $(OBJS:.o=.d)
LIB_BIN = $(BIN_DIR)/quickaest.so

APP_SRC := $(wildcard app/*.c)
APP_OBJS = $(OBJS)
APP_OBJS+= $(addprefix $(OBJ_DIR)/, $(APP_SRC:.c=.o))
APP_DEPS = $(APP_OBJS:.o=.d)
APP_EXEC = $(BIN_DIR)/app

BENCH_SRC := $(wildcard bench/*.c)
BENCH_OBJS = $(OBJS)
BENCH_OBJS+= $(addprefix $(OBJ_DIR)/, $(BENCH_SRC:.c=.o))
BENCH_DEPS = $(BENCH_OBJS:.o=.d)
BENCH_EXEC = $(BIN_DIR)/bench

TEST_SRC := $(wildcard tests/*.c)
TEST_OBJS = $(OBJS)
TEST_OBJS+= $(addprefix $(OBJ_DIR)/, $(TEST_SRC:.c=.o))
TEST_DEPS = $(TEST_OBJS:.o=.d)
TEST_EXEC = $(BIN_DIR)/run-tests

help:
	@echo 'Usage:'
	@echo '  make build-all'
	@echo '    Builds all components'
	@echo '  make <build-lib | build-app | build-bench | build-test>'
	@echo '    Builds the respective component'
	@echo '  make build-debug'
	@echo '    Builds the library in DEBUG mode'
	@echo '  make run'
	@echo '    Builds and runs the app'
	@echo '  make debug'
	@echo '    Builds the app in DEBUG mode and runs it in gdb'
	@echo '  make bench'
	@echo '    Builds and runs the benchmarks in quiet mode'
	@echo '  make bench-verbose'
	@echo '    Builds and runs benchmarks in verbose mode'
	@echo '  make test'
	@echo '    Builds and runs the test suite'
	@echo '  make clean'
	@echo '    Cleans up all object files and binaries in obj/ and bin/'
	@echo
	@echo 'The resulting binaries can be found in bin/, and the object files in obj/'
	@echo
	@echo 'Available env vars:'
	@echo '  CC         -- path to the C compiler used'
	@echo '  CFLAGS     -- flags to pass to the C compiler (ignored for benchmarks)'
	@echo '  LDFLAGS    -- flags to pass to the linker'
	@echo '  WARN       -- list of warnings to enable'
	@echo '  NO_WARN    -- list of warnings to disable'
	@echo
	@echo 'Available defines:'
	@echo '  VERBOSE    -- prints a bunch of debug info, like the state for every step'
	@echo '                of encryption/decryption'

# this doesn't build the benchmarks because those require cleaning & recompiling everything
# and since in theory there's no guarantee dependencies will be fulfilled, there's no way to
# ensure it's ran first or sequentially with the others
build-all: build-lib build-app build-test | $(BIN_DIR) $(OBJ_DIR)

build-lib: $(OBJS) | $(BIN_DIR) $(OBJ_DIR)
#	$(CC) -c -o $(LIB_BIN) $^ $(LDFLAGS)

build-app: $(APP_OBJS) | build-lib
	$(CC) -o $(APP_EXEC) $^ $(LDFLAGS)

build-bench: clean | build-bench_

build-bench_: LDFLAGS = -Ofast -march=native -flto
build-bench_: CFLAGS = $(WARNS) $(NO_WARNS) -Ofast -march=native -flto -fshort-enums
build-bench_: $(BENCH_OBJS) | $(BIN_DIR)
	$(CC) -o $(BENCH_EXEC) $^ $(LDFLAGS)

build-test: $(TEST_OBJS) | build-debug $(BIN_DIR)
	$(CC) -o $(TEST_EXEC) $^ $(LDFLAGS)

run: build-app
	@./$(APP_EXEC)

bench-verbose: build-bench
	@./$(BENCH_EXEC) -v

bench: build-bench
	@./$(BENCH_EXEC)

test: build-test
	@./$(TEST_EXEC)

app : build-app
	@./$(APP_EXEC)

build-debug: CFLAGS += -DDEBUG
build-debug: build-app

debug: build-debug
	@ASAN_OPTIONS="detect_leaks=0" gdb ./$(APP_EXEC)

clean:
	rm -rf $(BIN_DIR) $(OBJ_DIR)

# import auto-gen'd dependencies
-include $(DEPS)
-include $(APP_DEPS)
-include $(BENCH_DEPS)
-include $(TEST_DEPS)

# shamelessly stolen from http://scottmcpeak.com/autodepend/autodepend.html
# basically, uses a compiler feature that auto-generates dependency info
# to compile any .c file included in the project
$(OBJ_DIR)/%.o: %.c | $(OBJ_DIR) # '| $(OBJ_DIR)' makes sure the dir exists
	$(CC) -c  $(CFLAGS) $*.c -o $(OBJ_DIR)/$*.o
	$(CC) -MM $(CFLAGS) $*.c > $(OBJ_DIR)/$*.d
	@mv -f $(OBJ_DIR)/$*.d $(OBJ_DIR)/$*.d.tmp
	@sed -e 's|.*:|$(OBJ_DIR)/$*.o:|' < $(OBJ_DIR)/$*.d.tmp > $(OBJ_DIR)/$*.d
	@sed -e 's/.*://' -e 's/\\$$//' < $(OBJ_DIR)/$*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $(OBJ_DIR)/$*.d
	@rm -f $(OBJ_DIR)/$*.d.tmp

$(BIN_DIR):
	mkdir $(BIN_DIR)

$(OBJ_DIR):
	mkdir $(OBJ_DIR)
	mkdir $(OBJ_DIR)/src
	mkdir $(OBJ_DIR)/app
	mkdir $(OBJ_DIR)/bench
	mkdir $(OBJ_DIR)/tests
