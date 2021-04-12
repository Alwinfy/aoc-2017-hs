HCC=ghc
HCC_FLAGS=-O -g -Wall

OBJS=Day01.o Day02.o Day03.o Day04.o Day05.o Day06.o Day07.o Day08.o Day09.o Day10.o Day11.o Day12.o Day13.o Day14.o Day15.o Day16.o Day17.o Day18.o Day19.o Day20.o Day21.o Day22.o Day23.o Day24.o Day25.o

Main: Main.hs $(OBJS)
	$(HCC) $(HCC_FLAGS) $<

%.o: %.hs
	$(HCC) $(HCC_FLAGS) $<

clean:
	$(RM) *.o *.hi

.PHONY: clean
