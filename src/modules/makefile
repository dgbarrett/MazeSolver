CC = gfortran
FLAGS = -std='gnu' -Wall
COMPILE = $(CC) -c $(FLAGS)

all : Positions.o PositionStacks.o MazeSolving.o

MazeSolving.o : MazeSolving.f95
	$(COMPILE) MazeSolving.f95

PositionStacks.o : PositionStacks.f95
	$(COMPILE) PositionStacks.f95

Positions.o : Positions.f95
	$(COMPILE) Positions.f95

clean :
	rm -rf *.o
	rm -rf *mod
