/* The fastest CPU implementation of the N-body benchmark we could write:
 * NEON intrinsics over a structure of arrays, four bodies i per vector and
 * three vectors per block (register blocking: each body j that is
 * loaded is used for 12 interactions), 1/sqrt from the reciprocal
 * square-root estimate refined by one or two Newton steps, all multiply-adds
 * fused. With one argument "all" after n and steps, the blocks of bodies i are
 * spread over all cores with Grand Central Dispatch (dispatch_apply), whose
 * dynamic scheduling suits the mix of performance and efficiency cores.
 *
 * On the M4 Max, three vectors (12 bodies i per block) measured best (2 and
 * 4 are 10% slower). Two Newton steps give 1/sqrt to single precision (as
 * fast as vdivq(1, vsqrtq(r2)), within 5%); one step is
 * 14% faster but leaves a relative error of about 2e-5 in 1/r.
 *
 * Build: clang -O3 -mcpu=native -ffast-math nbody.c -o nbody_c
 *
 * Usage: nbody_c [n [steps [1|all [newton_steps]]]] */
#include <arm_neon.h>
#include <dispatch/dispatch.h>
#include <string.h>

#include "common.h"

enum { block_vectors = 3, block_size = 4 * block_vectors };

typedef struct {
    int n;
    const float *x, *y, *z, *m;
    float *ax, *ay, *az;
    int newton_steps;
} bodies_t;

static inline float32x4_t rsqrt(float32x4_t r2, int newton_steps)
{
    float32x4_t y = vrsqrteq_f32(r2);
    for (int k = 0; k < newton_steps; k++) {
        y = vmulq_f32(y, vrsqrtsq_f32(vmulq_f32(r2, y), y));
    }
    return y;
}

/* The accelerations of the bodies i0, ..., i0 + block_size - 1. */
static inline __attribute__((always_inline)) void
accel_block(const bodies_t *b, int i0, int newton_steps)
{
    const float32x4_t eps2 = vdupq_n_f32(nbody_eps2);
    float32x4_t xi[block_vectors], yi[block_vectors], zi[block_vectors];
    float32x4_t ax[block_vectors], ay[block_vectors], az[block_vectors];
    for (int k = 0; k < block_vectors; k++) {
        xi[k] = vld1q_f32(b->x + i0 + 4 * k);
        yi[k] = vld1q_f32(b->y + i0 + 4 * k);
        zi[k] = vld1q_f32(b->z + i0 + 4 * k);
        ax[k] = ay[k] = az[k] = vdupq_n_f32(0);
    }
    for (int j = 0; j < b->n; j++) {
        float32x4_t xj = vld1q_dup_f32(b->x + j);
        float32x4_t yj = vld1q_dup_f32(b->y + j);
        float32x4_t zj = vld1q_dup_f32(b->z + j);
        float32x4_t mj = vld1q_dup_f32(b->m + j);
        for (int k = 0; k < block_vectors; k++) {
            float32x4_t dx = vsubq_f32(xj, xi[k]);
            float32x4_t dy = vsubq_f32(yj, yi[k]);
            float32x4_t dz = vsubq_f32(zj, zi[k]);
            float32x4_t r2 = vfmaq_f32(eps2, dx, dx);
            r2 = vfmaq_f32(r2, dy, dy);
            r2 = vfmaq_f32(r2, dz, dz);
            float32x4_t inv_r = rsqrt(r2, newton_steps);
            float32x4_t s = vmulq_f32(vmulq_f32(mj, inv_r),
                vmulq_f32(inv_r, inv_r));
            ax[k] = vfmaq_f32(ax[k], dx, s);
            ay[k] = vfmaq_f32(ay[k], dy, s);
            az[k] = vfmaq_f32(az[k], dz, s);
        }
    }
    for (int k = 0; k < block_vectors; k++) {
        vst1q_f32(b->ax + i0 + 4 * k, ax[k]);
        vst1q_f32(b->ay + i0 + 4 * k, ay[k]);
        vst1q_f32(b->az + i0 + 4 * k, az[k]);
    }
}

static void accel_selected(const bodies_t *b, int i0)
{
    if (b->newton_steps == 1) accel_block(b, i0, 1);
    else accel_block(b, i0, 2);
}

/* The accelerations of the bodies past the last whole block. */
static void accel_scalar(const bodies_t *b, int i)
{
    float axi = 0, ayi = 0, azi = 0;
    for (int j = 0; j < b->n; j++) {
        float dx = b->x[j] - b->x[i], dy = b->y[j] - b->y[i],
            dz = b->z[j] - b->z[i];
        float inv_r = 1 / sqrtf(dx * dx + dy * dy + dz * dz + nbody_eps2);
        float s = b->m[j] * inv_r * inv_r * inv_r;
        axi += dx * s;
        ayi += dy * s;
        azi += dz * s;
    }
    b->ax[i] = axi;
    b->ay[i] = ayi;
    b->az[i] = azi;
}

static void accel(const bodies_t *b, int parallel)
{
    int blocks = b->n / block_size;
    if (parallel) {
        /* Chunks of a few blocks, many more than cores, for load balance. */
        enum { blocks_per_chunk = 4 };
        size_t chunks = (blocks + blocks_per_chunk - 1) / blocks_per_chunk;
        dispatch_apply(chunks, DISPATCH_APPLY_AUTO, ^(size_t c) {
            int end = (int)(c + 1) * blocks_per_chunk;
            if (end > blocks) end = blocks;
            for (int k = (int)c * blocks_per_chunk; k < end; k++) {
                accel_selected(b, k * block_size);
            }
        });
    } else {
        for (int k = 0; k < blocks; k++) accel_selected(b, k * block_size);
    }
    for (int i = blocks * block_size; i < b->n; i++) accel_scalar(b, i);
}

int main(int argc, char **argv)
{
    int n = 16384, steps = 10;
    nbody_args(argc, argv, &n, &steps);
    int parallel = argc >= 4 && strcmp(argv[3], "all") == 0;
    int newton_steps = argc >= 5 ? atoi(argv[4]) : 2;
    if (newton_steps != 1 && newton_steps != 2) {
        fprintf(stderr, "newton_steps must be 1 or 2\n");
        return 1;
    }

    float *x = malloc(10 * sizeof(float) * n);
    if (!x) {
        fprintf(stderr, "cannot allocate bodies\n");
        return 1;
    }
    float *y = x + n, *z = y + n, *vx = z + n, *vy = vx + n, *vz = vy + n,
        *m = vz + n, *ax = m + n, *ay = ax + n, *az = ay + n;
    nbody_init(n, x, y, z, vx, vy, vz, m);
    bodies_t b = {n, x, y, z, m, ax, ay, az, newton_steps};

    double e0 = nbody_energy(n, x, y, z, vx, vy, vz, m);
    double t0 = 0;
    /* The same kick-drift-kick steps as nbody.f90; step 0 is not timed. */
    for (int step = 0; step <= steps; step++) {
        if (step == 1) t0 = nbody_seconds();
        accel(&b, parallel);
        float kick = (step == 0 || step == steps) ? nbody_dt / 2 : nbody_dt;
        float drift = step == steps ? 0 : nbody_dt;
        for (int i = 0; i < n; i++) {
            vx[i] += kick * ax[i];
            vy[i] += kick * ay[i];
            vz[i] += kick * az[i];
            x[i] += drift * vx[i];
            y[i] += drift * vy[i];
            z[i] += drift * vz[i];
        }
    }
    double time = nbody_seconds() - t0;
    double e1 = nbody_energy(n, x, y, z, vx, vy, vz, m);
    nbody_report(n, steps, time, e0, e1, nbody_checksum(n, x, y, z));
    free(x);
    return 0;
}
