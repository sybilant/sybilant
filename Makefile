NASM ?= nasm
LD ?= ld
NASMFLAGS := -f elf64 -Wall -w-reloc-abs-qword -w-reloc-rel-dword

BUILD_DIR := build
LIB_OBJECTS := $(BUILD_DIR)/sybilant.o $(BUILD_DIR)/sybilant.array.o
TEST_SOURCES := $(wildcard test/*.asm)
TEST_OBJECTS := $(patsubst test/%.asm,$(BUILD_DIR)/test/%.o,$(TEST_SOURCES))
TEST_BINS := $(patsubst test/%.asm,$(BUILD_DIR)/test/%,$(TEST_SOURCES))

.PHONY: all test clean

all: $(LIB_OBJECTS)

test: $(TEST_BINS)
	@set -e; for test_bin in $(TEST_BINS); do \
		printf 'TEST %s\n' "$$test_bin"; \
		"$$test_bin"; \
	done

$(BUILD_DIR)/%.o: lib/%.asm
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

$(BUILD_DIR)/test/%.o: test/%.asm
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

$(BUILD_DIR)/test/%: $(BUILD_DIR)/test/%.o $(LIB_OBJECTS)
	$(LD) -z noexecstack -o $@ $^

clean:
	rm -rf $(BUILD_DIR)
