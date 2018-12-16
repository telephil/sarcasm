CC = clang
CFLAGS = -std=c11 -Wall -Werror -pedantic `pkg-config --cflags bdw-gc` -g -Ilinenoise
LDFLAGS = `pkg-config --libs bdw-gc` -lcord -lreadline

TARGET  = scm
SOURCES = $(wildcard src/*.c)

all:	$(SOURCES)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $(TARGET)

clean:
	rm -f $(TARGET) 

