# CC = /usr/local/bin/gcc-6
CPP = ~/Projects/ptiger/gcc-install/bin/g++
# CPP = g++
CC = ~/Projects/ptiger/gcc-install/bin/gcc
#CC = gcc
PTIGERCC = ~/Projects/ptiger/gcc-install/bin/gccptiger

CFLAGS = -std=gnu99 -lm
DATASET = -DLARGE_DATASET
PROGRAM = cholesky

lib:
	export DYLD_LIBRARY_PATH=../ && export LD_LIBRARY_PATH=../
	$(CPP) -fPIC -shared ../ptigerlib.c -o ../libptiger.so

ludec:
	$(PTIGERCC) -L../ -o ludec.out ludec.ptiger -lptiger

mm:
	$(PTIGERCC) -L../ -o mm.out mm.ptiger -lptiger

working:
	$(PTIGERCC) -L../ -o whatisworking.out whatisworking.ptiger -lptiger

clean:
	rm *.out ../*.so
