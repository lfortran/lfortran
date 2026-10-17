// One kick-drift-kick step of the N-body benchmark, the classic tiled
// algorithm of GPU Gems 3, chapter 31, tuned for Apple GPUs.
//
// Each thread owns BODIES_PER_THREAD bodies i (TILE apart, so that the
// threads of a SIMD-group read and write consecutive elements) and keeps
// their positions and accelerations in registers. The threadgroup sweeps
// over all bodies j in tiles of TILE bodies: every thread loads one body of
// the tile into threadgroup memory, and then all threads read the whole tile
// from there, each body j being used for BODIES_PER_THREAD interactions per
// read. A position is a float4 (x, y, z, mass), loaded with one instruction.
//
// The kernel also kicks the velocities and drifts the positions of its bodies
// (into a second buffer, as other threadgroups still read the old positions),
// which saves a second launch per step.
//
// The host defines TILE (the threadgroup size), BODIES_PER_THREAD,
// USE_THREADGROUP_MEMORY (0 reads the bodies j straight from device memory,
// relying on the cache, for comparison) and UNROLL before compiling this
// source. On the M4 Max, TILE = 128, BODIES_PER_THREAD = 1 and UNROLL = 4
// measured best; threadgroup memory gains only a few percent over the cache,
// as the threads of a SIMD-group all read the same body j at once. The kernel
// is then bound by the arithmetic: 12 FP32 operations and one rsqrt (which
// issues at a quarter of the FMA rate) per interaction.
#include <metal_stdlib>
using namespace metal;

struct step_params {
    uint n;
    float eps2;
    float kick;
    float drift;
};

kernel void nbody_step(device const float4 *pos_in [[buffer(0)]],
                       device float4 *pos_out [[buffer(1)]],
                       device float4 *vel [[buffer(2)]],
                       constant step_params &p [[buffer(3)]],
                       uint tid [[thread_position_in_threadgroup]],
                       uint group [[threadgroup_position_in_grid]])
{
    const uint first = group * TILE * BODIES_PER_THREAD + tid;
    float3 pi[BODIES_PER_THREAD];
    float3 acc[BODIES_PER_THREAD];
    for (uint k = 0; k < BODIES_PER_THREAD; k++) {
        pi[k] = pos_in[first + k * TILE].xyz;
        acc[k] = 0;
    }

#if USE_THREADGROUP_MEMORY
    threadgroup float4 tile[TILE];
#endif
    for (uint t = 0; t < p.n; t += TILE) {
#if USE_THREADGROUP_MEMORY
        tile[tid] = pos_in[t + tid];
        threadgroup_barrier(mem_flags::mem_threadgroup);
#else
        device const float4 *tile = pos_in + t;
#endif
#pragma unroll(UNROLL)
        for (uint j = 0; j < TILE; j++) {
            const float4 q = tile[j];
            for (uint k = 0; k < BODIES_PER_THREAD; k++) {
                const float3 d = q.xyz - pi[k];
                const float r2 =
                    fma(d.x, d.x, fma(d.y, d.y, fma(d.z, d.z, p.eps2)));
                const float inv_r = rsqrt(r2);
                const float s = q.w * inv_r * inv_r * inv_r;
                acc[k] = fma(d, s, acc[k]);
            }
        }
#if USE_THREADGROUP_MEMORY
        threadgroup_barrier(mem_flags::mem_threadgroup);
#endif
    }

    for (uint k = 0; k < BODIES_PER_THREAD; k++) {
        const uint i = first + k * TILE;
        float4 v = vel[i];
        v.xyz += p.kick * acc[k];
        vel[i] = v;
        const float4 x = pos_in[i];
        pos_out[i] = float4(x.xyz + p.drift * v.xyz, x.w);
    }
}
