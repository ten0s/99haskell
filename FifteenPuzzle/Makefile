HC=ghc
HFLAGS=-O

APP=Fifteen
SRC=TimeMeasure.hs ListAux.hs FifteenPuzzle.hs Fifteen.hs \

all:
	${HC} ${HFLAGS} ${SRC} -o ${APP}

run: all
	./${APP}.exe

clean:
	rm -vf *.o *.hi ${APP}.exe

