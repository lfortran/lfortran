#ifndef MEM_DEBUG_HEADER
#define MEM_DEBUG_HEADER
#include <stddef.h>

// Public API
void  *dbg_malloc (size_t size);
void  *dbg_calloc (size_t nmemb, size_t size);
void  *dbg_realloc(void *ptr, size_t size);
void   dbg_free   (void *ptr);

/* Print all live allocations (leaks) and summary stats. */
void dbg_report ();

/*  Macro overrides — only when MEMORY_DEBUG is defined AND we are not    */
/*  inside the debugger's own implementation file. */
/*  Use it when forcing memory debug (compiler's side and runtime-lib's side)  */
/*  Notice : --detect-leaks flag will have no effect -- always ON */

#if !defined(MEM_DEBUG_IMPL)
// #  define malloc(s)      dbg_malloc (s)
// #  define calloc(n, s)   dbg_calloc (n, s)
// #  define realloc(p, s)  dbg_realloc(p, s)
// #  define free(p)        dbg_free   (p)
#endif

#endif /* MEM_DEBUG_HEADER */
