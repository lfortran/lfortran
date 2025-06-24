
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

typedef struct {
    void *base_addr;  // Pointer to data
    size_t elem_len; 
    // int rank;
    // CFI_type_t type;
    // int attribute;
    // CFI_dim_t dim[CFI_MAX_RANK];
} CFI_cdesc_t;

char f_string00_fortran(char *s);
char f_string01_fortran(char s);
char f_string02_fortran(char* s, int n);
char f_string03_fortran(char* s);
char f_string04_fortran(char* s);
char f_string05_fortran(CFI_cdesc_t* s);

int f_string00_c_caller(){
    char s = 'H';
    char r = f_string00_fortran(&s);

    printf("C Side : return of `f_string00_fortran` is [%c]\n", r);
    printf("%C\n", s);
    if(s != 'X'){
        printf("C Terminating process\n");
        exit(1);
    }
}

int f_string01_c_caller(){
    char s = 'H';
    char r = f_string01_fortran(s);

    printf("C Side : return of `f_string01_fortran` is [%c]\n", r);
    printf("%c\n", s);
    if(s == 'X'){
        printf("C Terminating process\n");
        exit(1);
    }
}
int f_string02_c_caller(){
    char *s = (char*)malloc(4 * sizeof(char));
    memcpy(s, "ABC",4);
    char r = f_string02_fortran(s, 3);
    printf("C Side : return of `f_string02_fortran` is [%c]\n", r);
    printf("%s\n", s);
    if(s[0] != '2'){
        printf("C Terminating process\n");
        exit(1);
    }
}
int f_string03_c_caller(){
    char *s = (char*)malloc(4 * sizeof(char));
    memcpy(s, "BCD",4);
    char r = f_string03_fortran(s);
    printf("C Side : return of `f_string03_fortran` is [%c]\n", r);
    printf("%s\n", s);
    if(s[0] != '3'){
        printf("C Terminating process\n");
        exit(1);
    }
}
int f_string04_c_caller(){
    char *s = (char*)malloc(4 * sizeof(char));
    memcpy(s, "CDE",4);
    char r = f_string04_fortran(s);
    printf("C Side : return of `f_string04_fortran` is [%c]\n", r);
    printf("%s\n", s);
    if(s[0] != '4'){
        printf("C Terminating process\n");
        exit(1);
    }
}

int f_string05_c_caller(){
    char *s = (char*)malloc(4 * sizeof(char));
    memcpy(s, "DEF",4);
    CFI_cdesc_t desc = {s, sizeof(char)};
    char r = f_string05_fortran(&desc);
    printf("C Side : return of `f_string05_fortran` is [%c]\n", r);
    printf("%s\n", s);
    if(s[0] != '5'){
        printf("C Terminating process\n");
        exit(1);
    }
}
