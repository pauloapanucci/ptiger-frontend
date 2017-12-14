extern "C" {

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

void print_hello_ptiger() {
    printf("HELLO PTIGER, NOW YOU CAN USE FUNC FROM EXTERNAL SHARED LIB!\n");
}

}
