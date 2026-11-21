/* The array descriptor differs between compilers, so the array is not
   read. Both names are defined: GFortran appends an underscore to the
   names of external procedures, LFortran does not by default. */

void external_41_flag(void *a, int *flag)
{
    *flag = 42;
}

void external_41_flag_(void *a, int *flag)
{
    *flag = 42;
}
