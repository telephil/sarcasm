CC = clang
CFLAGS = -std=c11 -Wall -Werror -pedantic `pkg-config --cflags bdw-gc` -g
LDFLAGS = `pkg-config --libs bdw-gc` -lcord -lreadline

TARGET  = scm
SOURCES = $(wildcard src/*.c)

all:	$(TARGET)
	
$(TARGET):	$(SOURCES)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $(TARGET)

test:	$(TARGET)
	@./$(TARGET) ./tests/base.scm

clean:
	rm -f $(TARGET) 

