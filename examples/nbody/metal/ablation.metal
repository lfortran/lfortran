// Variants of the force kernel, from the kernel LFortran generates for the
// force loop of nbody.f90 to the tiled algorithm, all on the same structure
// of arrays, for the ablation in ANALYSIS.md. ablation.m runs each of them,
// followed by the update kernel LFortran generates, as nbody.f90 does.
#include <metal_stdlib>
using namespace metal;

struct force_args {
    int n;
    int lb;  // the lower bound of every array, 1
};

struct update_args {
    float drift;
    float kick;
    int n;
    int lb;
};

constant float eps2 = 9.99999977648258209e-03;

#if USE_RSQRT
#define INV_SQRT(r2) rsqrt(r2)
#else
#define INV_SQRT(r2) (1 / sqrt(r2))
#endif

// The kernel `lfortran --gpu=metal --show-gpu-kernel-source nbody.f90`
// prints, with the lower bounds and extents of the arrays folded into `lb`.
kernel void lfortran(device float* ax [[buffer(0)]],
                     device float* ay [[buffer(1)]],
                     device float* az [[buffer(2)]],
                     device float* m [[buffer(3)]],
                     device float* x [[buffer(4)]],
                     device float* y [[buffer(5)]],
                     device float* z [[buffer(6)]],
                     constant force_args& a [[buffer(7)]],
                     uint __thread_id [[thread_position_in_grid]])
{
    int n = a.n, __lb_ax_0 = a.lb, __lb_ay_0 = a.lb, __lb_az_0 = a.lb,
        __lb_m_0 = a.lb, __lb_x_0 = a.lb, __lb_y_0 = a.lb, __lb_z_0 = a.lb,
        __loop_end_0 = a.n;
    int __do_loop_end, __flat_idx, i, j;
    float axi, ayi, azi, dx, dy, dz, inv_r, r2, s, xi, yi, zi;
    if ((((0 * 0) + __thread_id) >= (((__loop_end_0 >= 1)) ? (((__loop_end_0 - 1) + 1)) : (0)))) {
        return;
    }
    __flat_idx = ((0 * 0) + __thread_id);
    i = (__flat_idx + 1);
    xi = x[((int)(i) - (__lb_x_0))];
    yi = y[((int)(i) - (__lb_y_0))];
    zi = z[((int)(i) - (__lb_z_0))];
    axi = ((float)0);
    ayi = ((float)0);
    azi = ((float)0);
    __do_loop_end = n;
    j = (1 - 1);
    while (((j + 1) <= __do_loop_end)) {
        j = (j + 1);
        dx = (x[((int)(j) - (__lb_x_0))] - xi);
        dy = (y[((int)(j) - (__lb_y_0))] - yi);
        dz = (z[((int)(j) - (__lb_z_0))] - zi);
        r2 = ((((dx * dx) + (dy * dy)) + (dz * dz)) + eps2);
        inv_r = (((float)1) / sqrt(r2));
        s = (((m[((int)(j) - (__lb_m_0))] * inv_r) * inv_r) * inv_r);
        axi = (axi + (dx * s));
        ayi = (ayi + (dy * s));
        azi = (azi + (dz * s));
    }
    ax[((int)(i) - (__lb_ax_0))] = axi;
    ay[((int)(i) - (__lb_ay_0))] = ayi;
    az[((int)(i) - (__lb_az_0))] = azi;
}

// A counted loop that the Metal compiler unrolls by UNROLL, with read-only
// arrays, and 1/sqrt written as INV_SQRT (1 / sqrt(r2) or rsqrt(r2)).
kernel void unrolled(device float* ax [[buffer(0)]],
                     device float* ay [[buffer(1)]],
                     device float* az [[buffer(2)]],
                     device const float* m [[buffer(3)]],
                     device const float* x [[buffer(4)]],
                     device const float* y [[buffer(5)]],
                     device const float* z [[buffer(6)]],
                     constant force_args& a [[buffer(7)]],
                     uint i [[thread_position_in_grid]])
{
    if ((int)i >= a.n) return;
    const float xi = x[i], yi = y[i], zi = z[i];
    float axi = 0, ayi = 0, azi = 0;
#pragma unroll(UNROLL)
    for (int j = 0; j < a.n; j++) {
        const float dx = x[j] - xi, dy = y[j] - yi, dz = z[j] - zi;
        const float r2 = dx * dx + dy * dy + dz * dz + eps2;
        const float inv_r = INV_SQRT(r2);
        const float s = m[j] * inv_r * inv_r * inv_r;
        axi += dx * s;
        ayi += dy * s;
        azi += dz * s;
    }
    ax[i] = axi;
    ay[i] = ayi;
    az[i] = azi;
}

// The same with the bodies j staged through threadgroup memory in tiles of
// the threadgroup size (GPU Gems 3, chapter 31), still a structure of arrays.
kernel void tiled(device float* ax [[buffer(0)]],
                  device float* ay [[buffer(1)]],
                  device float* az [[buffer(2)]],
                  device const float* m [[buffer(3)]],
                  device const float* x [[buffer(4)]],
                  device const float* y [[buffer(5)]],
                  device const float* z [[buffer(6)]],
                  constant force_args& a [[buffer(7)]],
                  uint i [[thread_position_in_grid]],
                  uint tid [[thread_position_in_threadgroup]])
{
    threadgroup float tx[TILE], ty[TILE], tz[TILE], tm[TILE];
    const float xi = x[i], yi = y[i], zi = z[i];
    float axi = 0, ayi = 0, azi = 0;
    for (int t = 0; t < a.n; t += TILE) {
        tx[tid] = x[t + tid];
        ty[tid] = y[t + tid];
        tz[tid] = z[t + tid];
        tm[tid] = m[t + tid];
        threadgroup_barrier(mem_flags::mem_threadgroup);
#pragma unroll(UNROLL)
        for (int j = 0; j < TILE; j++) {
            const float dx = tx[j] - xi, dy = ty[j] - yi, dz = tz[j] - zi;
            const float r2 = dx * dx + dy * dy + dz * dz + eps2;
            const float inv_r = INV_SQRT(r2);
            const float s = tm[j] * inv_r * inv_r * inv_r;
            axi += dx * s;
            ayi += dy * s;
            azi += dz * s;
        }
        threadgroup_barrier(mem_flags::mem_threadgroup);
    }
    ax[i] = axi;
    ay[i] = ayi;
    az[i] = azi;
}

// The update loop of nbody.f90, as LFortran generates it (simplified).
kernel void update(device float* ax [[buffer(0)]],
                   device float* ay [[buffer(1)]],
                   device float* az [[buffer(2)]],
                   device float* vx [[buffer(3)]],
                   device float* vy [[buffer(4)]],
                   device float* vz [[buffer(5)]],
                   device float* x [[buffer(6)]],
                   device float* y [[buffer(7)]],
                   device float* z [[buffer(8)]],
                   constant update_args& a [[buffer(9)]],
                   uint i [[thread_position_in_grid]])
{
    if ((int)i >= a.n) return;
    vx[i] = vx[i] + a.kick * ax[i];
    vy[i] = vy[i] + a.kick * ay[i];
    vz[i] = vz[i] + a.kick * az[i];
    x[i] = x[i] + a.drift * vx[i];
    y[i] = y[i] + a.drift * vy[i];
    z[i] = z[i] + a.drift * vz[i];
}
