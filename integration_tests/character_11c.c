#include <stdio.h>
#include <ISO_Fortran_binding.h>
char f_string01_c( char *str ){
    printf("C Side : f_string01_c called with input argument [ %s ]\n", str);
    str[1] = 'X';
    return str[0];
}

char f_string02_c( char str ){
    printf("C Side : f_string02_c called with input argument [ %C ]\n", str);
    return str;
}

char f_string03_c( char *str ){
    printf("C Side : f_string03_c called with input argument [ %s ]\n", str);
    str[1] = 'X';
    return str[0];
}

char f_string04_c( char *str ){
    printf("C Side : f_string04_c called with input argument [ %s ]\n", str);
    str[1] = 'X';
    return str[0];
}

char f_string05_c( CFI_cdesc_t *str ){
    printf("C Side : f_string05_c called with input argument [ %s ]\n", (char*)str->base_addr);
    ((char*)str->base_addr)[1] = 'X';
    return ((char*)str->base_addr)[0];
}
