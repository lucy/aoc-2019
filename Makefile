CFLAGS += -O2 -Wall -Wextra -g
CPPFLAGS += -D_GNU_SOURCE
bin = $(patsubst %.c,%,$(wildcard *.c))
all: $(bin)
clean:; rm $(bin)
.PHONY: clean
