CC := ghc
# SRCS := $(s)
# SRCS := $(shell find . -iname "*.hs" -type f)
SRCS := $(wildcard *.hs)
TARGET := $(basename $(SRCS))

all: $(TARGET)

$(TARGET): %: %.hs
	$(CC) -o $@ $<

.PHONY: clean
clean:
	rm -rf *.o *.hi $(TARGET)
