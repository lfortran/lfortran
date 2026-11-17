#include <stdint.h>
#include <stdlib.h>

void __module_prif_prif_init(int32_t *stat)
{
    *stat = 0;
}

void __module_prif_prif_allocate_coarray(
    void *lower, void *upper, int64_t *bytes, void *cleanup, void *handle,
    void **data, void *coshape, void *team, char *errmsg, void *errmsg_len,
    void *errmsg_desc, void *stat, int64_t source)
{
    (void)lower; (void)upper; (void)cleanup; (void)handle; (void)coshape;
    (void)team; (void)errmsg; (void)errmsg_len; (void)errmsg_desc;
    (void)stat; (void)source;
    *data = calloc(1, *bytes);
}

void __module_prif_prif_sync_all(
    void *team, void *stat, char *errmsg, void *errmsg_len,
    void *errmsg_desc, void *source_stat, int64_t source)
{
    (void)team; (void)stat; (void)errmsg; (void)errmsg_len;
    (void)errmsg_desc; (void)source_stat; (void)source;
}

void __module_prif_prif_stop(
    char *message, int32_t *message_len, int32_t *code, char *quiet,
    int32_t *image, int64_t source)
{
    (void)message; (void)message_len; (void)quiet; (void)image; (void)source;
    exit(*code);
}
