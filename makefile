all : mazes

mazes : clean
	cd src; make

clean :
	cd src; make clean
	rm -rf *.exe
	rm -rf *.out
