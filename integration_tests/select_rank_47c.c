#include <stdint.h>

int64_t select_rank_47_sum(const int64_t *buf, int n)
{
    int64_t s = 0;
    for (int i = 0; i < n; i++) s = s * 10 + buf[i];
    return s;
}
