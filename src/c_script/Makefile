FLAGS = -Wall -g 
OFILE = main.o analysis.o
HFILE = analysis.h

prog : ${OFILE}
	gcc ${FLAGS} -o $@ $^ -lpcap

%.o : %.c ${HFILE}
	gcc ${FLAGS} -c $<

clean :
	rm *.o prog
