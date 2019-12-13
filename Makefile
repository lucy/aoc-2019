CFLAGS += -O2 -Wall -Wextra -g
CPPFLAGS += -D_GNU_SOURCE
cbin = $(patsubst %.c,%c,$(wildcard *.c))
hsbin = $(patsubst %.hs,%hs,$(wildcard *.hs))
gobin = $(patsubst %.go,%go,$(wildcard *.go))
all: $(cbin) $(hsbin) $(gobin)
%c: %.c; $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $^ -o $@
%hs: %.hs; ghc -outputdir tmp/$@ -O2 $^ -o $@
%go: %.go; go build -o $@ $^
clean:; rm -rf -- tmp $(cbin) $(hsbin) $(gobin) *.o *.hi
.PHONY: clean
