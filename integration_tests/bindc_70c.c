extern char fortran_explicit(void);
extern char fortran_default(void);
extern char fortran_implicit(void);

char c_explicit(void) { return 'q'; }
char c_default(void) { return 'r'; }
char c_implicit(void) { return 's'; }
char c_prefix(void) { return 't'; }

int bindc_70_check(void) {
    if (fortran_explicit() != 'b') return 1;
    if (fortran_default() != 'd') return 2;
    if (fortran_implicit() != 'i') return 3;
    return 0;
}
