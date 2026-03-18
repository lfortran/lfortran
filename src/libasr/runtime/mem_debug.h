#ifndef MEM_DEBUG_HEADER
#define MEM_DEBUG_HEADER
#include <stddef.h>
#include <stdint.h>

// Public API
void  *dbg_malloc (void *context, int64_t size);
void  *dbg_calloc (void *context, int64_t nmemb, int64_t size);
void  *dbg_realloc(void *context, void *ptr, int64_t size);
void   dbg_free   (void *context, void *ptr);

/* Print all live allocations (leaks) and summary stats. */
void dbg_report ();


#endif /* MEM_DEBUG_HEADER */
