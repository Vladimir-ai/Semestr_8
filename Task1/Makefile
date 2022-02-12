include ../Makefile.common

SRC = task_1.c
OBJ = ${SRC:.c=.o}

all: run

$(OBJ) : $(SRC)
	$(C_COMPILE)

run: $(OBJ)
	$(CC) -o $@ $^ $(LINK_FLAGS)

BINS = run

clean:
	$(RM) $(BINS) $(OBJ) *.map

rebuild: clean all
