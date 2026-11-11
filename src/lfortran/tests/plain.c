#include <stdlib.h>

int plain_global = 5;

int plain(void)
{
    return 7;
}

// Reading a global and calling libc resolve through relocations in the loaded
// code, so the load paths are exercised and not just a direct call.
int plain_reloc(void)
{
    return plain_global + abs(-4);
}
