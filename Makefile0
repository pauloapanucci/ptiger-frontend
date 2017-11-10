# CC = /usr/local/bin/gcc-6
CC = gcc
CFLAGS = -std=gnu99 -lm
DATASET = -DLARGE_DATASET
PROGRAM = cholesky

compile:
	$(CC) $(CFLAGS) -I utilities utilities/polybench.c $(PROGRAM).c $(DATASET) -o $(PROGRAM).out
	$(CC) $(CFLAGS) -I utilities utilities/polybench.c $(PROGRAM)_developed.c $(DATASET) -o $(PROGRAM)_developed.out
	$(CC) $(CFLAGS) -fopenmp -I utilities utilities/polybench.c $(PROGRAM)_omp.c $(DATASET) -o $(PROGRAM)_omp.out
	$(CC) $(CFLAGS) -pthread -I utilities utilities/polybench.c pthread_barrier.c $(PROGRAM)_pthread.c $(DATASET) -o $(PROGRAM)_pthread.out

clean:
	rm *.out
