BUILD_ROOT := build
TEST_BUILD_DIR := $(BUILD_ROOT)/test

RUNTIME_SOURCES := $(sort $(shell find lib -type f -name '*.asm'))
TEST_SOURCES := $(sort $(shell find test -type f -name '*_test.asm'))

RUNTIME_OBJECTS := \
  $(patsubst %.asm,$(TEST_BUILD_DIR)/objects/%.o,$(RUNTIME_SOURCES))
TEST_OBJECTS := \
  $(patsubst %.asm,$(TEST_BUILD_DIR)/objects/%.o,$(TEST_SOURCES))
TEST_EXECUTABLES := \
  $(patsubst test/%.asm,$(TEST_BUILD_DIR)/executables/%,$(TEST_SOURCES))
DEPENDENCY_FILES := $(RUNTIME_OBJECTS:.o=.d) $(TEST_OBJECTS:.o=.d)

.DEFAULT_GOAL := build-tests
.DELETE_ON_ERROR:
.SECONDARY: $(RUNTIME_OBJECTS) $(TEST_OBJECTS)

.PHONY: build-tests clean

build-tests: $(TEST_EXECUTABLES)

clean:
	rm -rf "$(BUILD_ROOT)"

$(TEST_BUILD_DIR)/objects/%.o: %.asm
	@mkdir -p "$(@D)"
	@printf 'NASM %s\n' "$<"
	@nasm -f elf64 -w+error -I ./ \
		-MD "$(@:.o=.d)" -MQ "$@" -MP \
		-o "$@" "$<"

$(TEST_BUILD_DIR)/executables/%: \
    $(TEST_BUILD_DIR)/objects/test/%.o $(RUNTIME_OBJECTS)
	@mkdir -p "$(@D)"
	@printf 'LD   %s\n' "$@"
	@ld --fatal-warnings --entry _start -o "$@" \
		$(RUNTIME_OBJECTS) "$<"

-include $(DEPENDENCY_FILES)
