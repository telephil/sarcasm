CC = clang
CFLAGS = -std=c11 -Wall -Werror -pedantic `pkg-config --cflags bdw-gc libffi` -g
LDFLAGS = `pkg-config --libs bdw-gc libffi` -lcord -lreadline -lgmp

TARGET  = scm
SOURCES = $(wildcard src/*.c)

all:	$(TARGET)
	
$(TARGET):	$(SOURCES)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $(TARGET)

test:	$(TARGET)
	@./$(TARGET) -l ./tests/r7rs.scm

clean:
	rm -f $(TARGET) 

