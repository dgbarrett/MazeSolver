CC = gfortran
FLAGS = -std='gnu' -Wall -Imodules
COMPILE = $(CC) -c $(FLAGS)
LINK = $(CC) $(FLAGS) -o

all : mazes

mazes : main.f95 
	cd modules; make
	$(LINK) \
					../mazes \
					  main.f95 \
					    modules/PositionStacks.o \
					    modules/MazeSolving.o \
					    modules/Positions.o

clean :
	cd modules; make clean
	rm -rf *.stackdump
