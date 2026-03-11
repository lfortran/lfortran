// C code: stores a pointer to a struct (does NOT copy it),
// then later dereferences it.
#include <stdlib.h>

typedef int (*callback_fn)(void *ctx);

typedef struct {
    callback_fn start;
    callback_fn finish;
} callbacks_t;

typedef struct {
    const callbacks_t *cbs;
    void *ctx;
} handle_t;

handle_t *my_alloc(const callbacks_t *cbs, void *ctx) {
    handle_t *h = (handle_t *)malloc(sizeof(handle_t));
    h->cbs = cbs;
    h->ctx = ctx;
    return h;
}

void my_free(handle_t *h) { free(h); }

int my_run(handle_t *h) {
    return h->cbs->start(h->ctx);
}
