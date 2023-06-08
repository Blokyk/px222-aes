CC ?= gcc
WARNS += -Wall -Wextra
NO_WARNS += -Wno-unused-value -Wno-format-extra-args
CFLAGS += $(WARNS) $(NO_WARNS)
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
APP_DEPS+= $(APP_OBJS:.o=.d)
APP_EXEC = $(BIN_DIR)/main

BENCH_SRC := $(wildcard bench/*.c)
BENCH_OBJS = $(OBJS)
BENCH_OBJS+= $(addprefix $(OBJ_DIR)/, $(BENCH_SRC:.c=.o))
BENCH_DEPS+= $(BENCH_OBJS:.o=.d)
BENCH_EXEC = $(BIN_DIR)/bench

TEST_SRC := $(wildcard tests/*.c)
TEST_OBJS = $(OBJS)
TEST_OBJS+= $(addprefix $(OBJ_DIR)/, $(TEST_SRC:.c=.o))
TEST_DEPS+= $(TEST_OBJS:.o=.d)
TEST_EXEC = $(BIN_DIR)/run-tests

build-all: build-lib build-app build-test

build-lib: $(OBJS) | $(BIN_DIR)
#	$(CC) -c -o $(LIB_BIN) $^ $(LDFLAGS)

build-app: $(APP_OBJS) | build-lib
	$(CC) -o $(APP_EXEC) $^ $(LDFLAGS)

build-bench: clean
	make build-bench_

build-bench_: LDFLAGS = -Ofast -march=native -flto
build-bench_: CFLAGS = $(WARNS) $(NO_WARNS) -Ofast -march=native -flto -fshort-enums
build-bench_: $(BENCH_OBJS)
	$(CC) -o $(BENCH_EXEC) $^ $(LDFLAGS)

build-test: $(TEST_OBJS) | build-debug
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
build-debug: build-lib

debug: build-debug
	@ASAN_OPTIONS="detect_leaks=0" gdb ./$(APP_EXEC)

clean:
	rm -rf $(OBJ_DIR) $(OBJS) $(DEPS) $(APP_OBJS) $(APP_DEPS) $(APP_EXEC) $(BENCH_DEPS) $(BENCH_EXEC) $(TEST_OBJS) $(TEST_DEPS) $(TEST_EXEC)

-include $(DEPS) # import auto-gen'd dependencies
-include $(TEST_DEPS) # import auto-gen'd dependencies

# shamelessly stolen from http://scottmcpeak.com/autodepend/autodepend.html
$(OBJ_DIR)/%.o: %.c | $(OBJ_DIR) # the '| $(OBJ_DIR)' thing makes sure the dir exists
	$(CC) -c $(CFLAGS) $*.c -o $(OBJ_DIR)/$*.o
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
