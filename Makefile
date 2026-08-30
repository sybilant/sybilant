NASM ?= nasm
LD ?= ld
NASMFLAGS := -f elf64 -Wall -w-reloc-abs-qword -w-reloc-rel-dword

BUILD_DIR := build
CONSTANTS := lib/sybilant.constants.asm
LIB_OBJECTS := $(BUILD_DIR)/sybilant.start.o $(BUILD_DIR)/sybilant.o $(BUILD_DIR)/sybilant.array.o \
	$(BUILD_DIR)/sybilant.atom.o $(BUILD_DIR)/sybilant.thread.o \
	$(BUILD_DIR)/sybilant.mutable.array.o
TEST_SOURCES := $(wildcard test/*.asm)
TEST_OBJECTS := $(patsubst test/%.asm,$(BUILD_DIR)/test/%.o,$(TEST_SOURCES))
TEST_BINS := $(patsubst test/%.asm,$(BUILD_DIR)/test/%,$(TEST_SOURCES))
TEST_LDFLAGS := -z noexecstack \
	--defsym=sybilant_tls_base=ADDR\(.tdata\)+SIZEOF\(.tdata\)

.PHONY: all test clean

all: $(LIB_OBJECTS)

test: $(TEST_BINS)
	@set -e; for test_bin in $(TEST_BINS); do \
		printf 'TEST %s\n' "$$test_bin"; \
		"$$test_bin"; \
	done

$(BUILD_DIR)/%.o: lib/%.asm $(CONSTANTS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

$(BUILD_DIR)/test/%.o: test/%.asm $(CONSTANTS)
	@mkdir -p $(@D)
	$(NASM) $(NASMFLAGS) -o $@ $<

$(BUILD_DIR)/test/%: $(BUILD_DIR)/test/%.o $(LIB_OBJECTS)
	$(LD) $(TEST_LDFLAGS) -o $@ $^

clean:
	rm -rf $(BUILD_DIR)
