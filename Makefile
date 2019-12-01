CFLAGS += -O2 -Wall -Wextra -g
CPPFLAGS += -D_GNU_SOURCE
all: $(patsubst %.c,%,$(wildcard *.c))
