/* Shared parts of the C and Metal N-body benchmarks: the initial conditions,
 * the energy check and the report. They match nbody.f90 bit for bit in the
 * initial conditions, so that every implementation starts from the same
 * bodies and the checksums can be compared. */
#ifndef NBODY_COMMON_H
#define NBODY_COMMON_H

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static const float nbody_dt = 0.001f;
static const float nbody_eps2 = 0.01f;
static const double nbody_flops_per_interaction = 20;

/* A uniform number in [0, 1) from a 31-bit linear congruential generator. */
static inline double nbody_uniform(int64_t *seed)
{
    *seed = (1103515245 * *seed + 12345) % 2147483648LL;
    return (double)*seed / 2147483648.0;
}

/* Uniformly random bodies in the cube [-1, 1]^3 with small random velocities
 * and masses, in the order nbody.f90 draws them. */
static inline void nbody_init(int n, float *x, float *y, float *z,
    float *vx, float *vy, float *vz, float *m)
{
    int64_t seed = 42;
    for (int i = 0; i < n; i++) {
        x[i] = (float)(2 * nbody_uniform(&seed) - 1);
        y[i] = (float)(2 * nbody_uniform(&seed) - 1);
        z[i] = (float)(2 * nbody_uniform(&seed) - 1);
        vx[i] = (float)(0.1 * (2 * nbody_uniform(&seed) - 1));
        vy[i] = (float)(0.1 * (2 * nbody_uniform(&seed) - 1));
        vz[i] = (float)(0.1 * (2 * nbody_uniform(&seed) - 1));
        m[i] = (float)((0.5 + nbody_uniform(&seed)) / n);
    }
}

/* The total (kinetic plus softened potential) energy, in double precision. */
static inline double nbody_energy(int n, const float *x, const float *y,
    const float *z, const float *vx, const float *vy, const float *vz,
    const float *m)
{
    double e = 0;
    for (int i = 0; i < n; i++) {
        e += 0.5 * m[i] * ((double)vx[i] * vx[i] + (double)vy[i] * vy[i]
            + (double)vz[i] * vz[i]);
        double pot = 0;
        for (int j = i + 1; j < n; j++) {
            double dx = (double)x[j] - x[i];
            double dy = (double)y[j] - y[i];
            double dz = (double)z[j] - z[i];
            pot += m[j] / sqrt(dx * dx + dy * dy + dz * dz + nbody_eps2);
        }
        e -= m[i] * pot;
    }
    return e;
}

static inline double nbody_checksum(int n, const float *x, const float *y,
    const float *z)
{
    double c = 0;
    for (int i = 0; i < n; i++) c += fabs(x[i]) + fabs(y[i]) + fabs(z[i]);
    return c / n;
}

static inline double nbody_seconds(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + 1e-9 * t.tv_nsec;
}

static inline void nbody_report(int n, int steps, double time, double e0,
    double e1, double checksum)
{
    double interactions = (double)n * n * steps;
    double gflops = nbody_flops_per_interaction * interactions / time / 1e9;
    double drift = fabs((e1 - e0) / e0);
    printf("N = %d  steps = %d\n", n, steps);
    printf("time = %10.4f s\n", time);
    printf("interactions/s = %10.3E\n", interactions / time);
    printf("GFLOP/s = %10.1f\n", gflops);
    printf("energy: E0 = %16.8E  E1 = %16.8E  rel. drift = %9.2E\n", e0, e1,
        drift);
    printf("checksum = %12.8f\n", checksum);
    printf("RESULT %d %d %12.6f %10.1f %9.2E %12.8f\n", n, steps, time, gflops,
        drift, checksum);
}

static inline void nbody_args(int argc, char **argv, int *n, int *steps)
{
    if (argc >= 2) *n = atoi(argv[1]);
    if (argc >= 3) *steps = atoi(argv[2]);
    if (*n <= 0 || *steps <= 0) {
        fprintf(stderr, "usage: %s [n [steps]]\n", argv[0]);
        exit(1);
    }
}

#endif
