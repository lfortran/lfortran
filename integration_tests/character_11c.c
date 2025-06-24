#include <stdio.h>
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

typedef struct {
    void *base_addr;  // Pointer to data
    // size_t elem_len; 
    // int rank;
    // CFI_type_t type;
    // int attribute;
    // CFI_dim_t dim[CFI_MAX_RANK];
} CFI_cdesc_t;

char f_string05_c( CFI_cdesc_t *str ){
    printf("C Side : f_string05_c called with input argument [ %s ]\n", (char*)str->base_addr);
    ((char*)str->base_addr)[1] = 'X';
    return ((char*)str->base_addr)[0];
}
