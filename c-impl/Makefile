CC ?= gcc
CFLAGS += -march=native -Wall -Wextra -Wno-format-extra-args -g -flto -fshort-enums -fsanitize=address,leak
LDFLAGS += -lcrypt -flto -fsanitize=address,leak

OBJ_DIR = obj

SRC := $(wildcard src/*.c)
OBJS = $(addprefix $(OBJ_DIR)/, $(SRC:.c=.o))
DEPS = $(OBJS:.o=.d)
EXEC = main

TEST_SRC = $(SRC)
TEST_SRC += $(wildcard tests/*.c)
TEST_OBJS = $(OBJS)
TEST_OBJS += $(addprefix $(OBJ_DIR)/, $(TEST_SRC:.c=.o))
TEST_DEPS = DEPS
TEST_DEPS += $(TEST_OBJS:.o=.d)
TEST_EXEC = test-main

all: build-lib build-app build-test

build-app: $(APP_OBJS)
    $(CC) -o $(APP) $^ $(LDFLAGS)

build-lib: $(OBJS)
	$(CC) -o $(EXEC) $^ $(LDFLAGS)

build-test: $(TEST_OBJS)
	$(CC) -o $(TEST_EXEC) $^ $(LDFLAGS)

run: build
	@./$(EXEC)

test: build-test
	@./$(TEST_EXEC)

build-debug: CFLAGS += -DDEBUG
build-debug: clean | build

debug: build-debug
	@ASAN_OPTIONS="detect_leaks=0" gdb ./$(EXEC)

clean:
	rm -rf $(OBJ_DIR) $(OBJS) $(DEPS) $(EXEC) $(TEST_OBJS) $(TEST_DEPS) $(TEST_EXEC)

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

$(OBJ_DIR):
	mkdir $(OBJ_DIR)
	mkdir $(OBJ_DIR)/src
	mkdir $(OBJ_DIR)/tests