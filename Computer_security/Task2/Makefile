include ../Makefile.common

INCPATH = -I ../Common

SRC = task_2.c \
	cipher_text.c \
	 ../Common/common.c

OBJ = ${SRC:.c=.o}

all: run

%.o : %.c
	$(C_COMPILE)

run: $(OBJ)
	$(CC) -o $@ $^ $(LINK_FLAGS)

BINS = run

clean:
	$(RM) $(BINS) $(OBJ) *.map

rebuild: clean all
