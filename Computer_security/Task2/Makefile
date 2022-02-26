include ../Makefile.common

INCPATH = -I ../Common

SRC = task_2.c \
	cipher_text.c \
	 ../Common/common.c

SRC2 = final_check.c \
	cipher_text.c \
	 ../Common/common.c

OBJ = ${SRC:.c=.o}
OBJ2 = ${SRC2:.c=.o}

all: check \
	run

%.o : %.c
	$(C_COMPILE)

run: $(OBJ)
	$(CC) -o $@ $^ $(LINK_FLAGS)

check: $(OBJ2)
	$(CC) -o $@ $^ $(LINK_FLAGS)

BINS = run check

clean:
	$(RM) $(BINS) $(OBJ) $(OBJ2) *.map

rebuild: clean all
