extern "C" {

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void power_print(int a, int b){
    printf("%d\n", pow(a, b));
}

void print_integer(int n) {
    printf("%d", n);
}

void print_string(char *s) {
    printf("%s", s);
}

void print_real(float n) {
    printf("%.2f", n);
}

void println() {
    printf("\n");
}

void print_matrix_integer(int **m, int line, int column) {
    for (int i = 0; i < line; i++) {
        for (int j = 0; j < column; j++) {
            printf(" %d ", m[i][j]);
        }
        printf("\n");
    }
}

void print_matrix_real(int **m, int line, int column) {
    for (int i = 0; i < line; i++) {
        for (int j = 0; j < column; j++) {
            printf(" %f ", m[i][j]);
        }
        printf("\n");
    }
}

void print_array_integer(int a[], int size) {
    for (int i = 0; i < size; i++) {
        printf(" %d ", a[i]);
    }
}


void print_hello_ptiger() {
    printf("HELLO PTIGER, NOW YOU CAN USE FUNC FROM EXTERNAL SHARED LIB!\n");
}

}
