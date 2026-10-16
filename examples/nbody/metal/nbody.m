// Host of the hand-written Metal N-body benchmark (Objective-C).
//
// The kernel source (nbody.metal) is compiled at run time with
// newLibraryWithSource:, which needs no Xcode (only the Command Line Tools).
// Positions and masses are float4 (x, y, z, m), velocities float4 (w unused),
// in shared (unified) memory. Every step is one dispatch; all timed steps are
// encoded and committed without waiting, and the host waits once at the end.
//
// Usage: nbody_metal [n [steps [tile [bodies_per_thread
//                   [threadgroup_memory [unroll]]]]]]
//
// Build: clang -O2 -fobjc-arc -framework Metal -framework Foundation
//            -DNBODY_METAL_SOURCE='"path/to/nbody.metal"' nbody.m
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "../c/common.h"

typedef struct {
    uint32_t n;
    float eps2;
    float kick;
    float drift;
} step_params;

static void fail(NSString *what, NSError *error)
{
    fprintf(stderr, "%s: %s\n", what.UTF8String,
        error ? error.localizedDescription.UTF8String : "");
    exit(1);
}

static void energy_and_checksum(int n, const float *pos, const float *vel,
    double *e, double *checksum)
{
    float *s = malloc(7 * sizeof(float) * n);
    if (!s) fail(@"cannot allocate energy workspace", nil);
    float *x = s, *y = x + n, *z = y + n, *vx = z + n, *vy = vx + n,
        *vz = vy + n, *m = vz + n;
    for (int i = 0; i < n; i++) {
        x[i] = pos[4 * i];
        y[i] = pos[4 * i + 1];
        z[i] = pos[4 * i + 2];
        m[i] = pos[4 * i + 3];
        vx[i] = vel[4 * i];
        vy[i] = vel[4 * i + 1];
        vz[i] = vel[4 * i + 2];
    }
    *e = nbody_energy(n, x, y, z, vx, vy, vz, m);
    if (checksum) *checksum = nbody_checksum(n, x, y, z);
    free(s);
}

int main(int argc, char **argv)
{
    @autoreleasepool {
        int n = 8192, steps = 5;
        nbody_args(argc > 3 ? 3 : argc, argv, &n, &steps);
        int tile = argc >= 4 ? atoi(argv[3]) : 128;
        int bodies_per_thread = argc >= 5 ? atoi(argv[4]) : 1;
        int threadgroup_memory = argc >= 6 ? atoi(argv[5]) : 1;
        int unroll = argc >= 7 ? atoi(argv[6]) : 4;
        if (tile < 1 || tile > 1024 || bodies_per_thread < 1 ||
            bodies_per_thread > 4 || unroll < 1 || unroll > 16 ||
            (threadgroup_memory != 0 && threadgroup_memory != 1)) {
            fprintf(stderr, "invalid tile, bodies_per_thread, memory mode or unroll\n");
            return 1;
        }
        if (n % (tile * bodies_per_thread) != 0) {
            fprintf(stderr, "n must be a multiple of tile * bodies_per_thread"
                " = %d\n", tile * bodies_per_thread);
            return 1;
        }

        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) fail(@"no Metal device", nil);
        NSError *error = nil;
        NSString *source = [NSString
            stringWithContentsOfFile:@NBODY_METAL_SOURCE
                            encoding:NSUTF8StringEncoding
                               error:&error];
        if (!source) fail(@"cannot read " @NBODY_METAL_SOURCE, error);
        MTLCompileOptions *options = [MTLCompileOptions new];
        options.preprocessorMacros = @{
            @"TILE" : @(tile),
            @"BODIES_PER_THREAD" : @(bodies_per_thread),
            @"USE_THREADGROUP_MEMORY" : @(threadgroup_memory),
            @"UNROLL" : @(unroll),
        };
        id<MTLLibrary> library = [device newLibraryWithSource:source
                                                      options:options
                                                        error:&error];
        if (!library) fail(@"cannot compile the kernel", error);
        id<MTLComputePipelineState> pipeline = [device
            newComputePipelineStateWithFunction:
                [library newFunctionWithName:@"nbody_step"]
                                          error:&error];
        if (!pipeline) fail(@"cannot create the pipeline", error);
        if ((NSUInteger)tile > pipeline.maxTotalThreadsPerThreadgroup) {
            fprintf(stderr, "tile %d exceeds the maximum threadgroup size "
                "%lu of this kernel\n", tile,
                (unsigned long)pipeline.maxTotalThreadsPerThreadgroup);
            return 1;
        }

        size_t bytes = sizeof(float) * 4 * n;
        id<MTLBuffer> pos[2] = {
            [device newBufferWithLength:bytes
                                options:MTLResourceStorageModeShared],
            [device newBufferWithLength:bytes
                                options:MTLResourceStorageModeShared]};
        id<MTLBuffer> vel = [device
            newBufferWithLength:bytes
                        options:MTLResourceStorageModeShared];
        if (!pos[0] || !pos[1] || !vel) fail(@"cannot allocate device buffers", nil);
        {
            float *s = malloc(7 * sizeof(float) * n);
            if (!s) fail(@"cannot allocate initial conditions", nil);
            float *x = s, *y = x + n, *z = y + n, *vx = z + n, *vy = vx + n,
                *vz = vy + n, *m = vz + n;
            nbody_init(n, x, y, z, vx, vy, vz, m);
            float *p = pos[0].contents, *v = vel.contents;
            for (int i = 0; i < n; i++) {
                p[4 * i] = x[i];
                p[4 * i + 1] = y[i];
                p[4 * i + 2] = z[i];
                p[4 * i + 3] = m[i];
                v[4 * i] = vx[i];
                v[4 * i + 1] = vy[i];
                v[4 * i + 2] = vz[i];
                v[4 * i + 3] = 0;
            }
            free(s);
        }
        double e0;
        energy_and_checksum(n, pos[0].contents, vel.contents, &e0, NULL);

        id<MTLCommandQueue> queue = [device newCommandQueue];
        if (!queue) fail(@"cannot create command queue", nil);
        MTLSize grid = MTLSizeMake(n / bodies_per_thread, 1, 1);
        MTLSize group = MTLSizeMake(tile, 1, 1);
        // Steps per command buffer: committing costs the host some
        // microseconds, a command buffer that runs for seconds risks the
        // GPU watchdog.
        const int steps_per_buffer = 16;
        int current = 0;
        id<MTLCommandBuffer> buffer = nil;
        id<MTLComputeCommandEncoder> encoder = nil;
        NSMutableArray<id<MTLCommandBuffer>> *submitted = [NSMutableArray new];
        double t0 = 0;
        // The same kick-drift-kick steps as nbody.f90; step 0 is not timed.
        for (int step = 0; step <= steps; step++) {
            if (step == 1) {
                [buffer waitUntilCompleted];
                if (buffer.error) fail(@"initial step failed", buffer.error);
                t0 = nbody_seconds();
            }
            if (!encoder) {
                buffer = [queue commandBuffer];
                encoder = [buffer computeCommandEncoder];
                [encoder setComputePipelineState:pipeline];
                [encoder setBuffer:vel offset:0 atIndex:2];
            }
            step_params params = {(uint32_t)n, nbody_eps2,
                (step == 0 || step == steps) ? nbody_dt / 2 : nbody_dt,
                step == steps ? 0 : nbody_dt};
            [encoder setBuffer:pos[current] offset:0 atIndex:0];
            [encoder setBuffer:pos[1 - current] offset:0 atIndex:1];
            [encoder setBytes:&params length:sizeof(params) atIndex:3];
            [encoder dispatchThreads:grid threadsPerThreadgroup:group];
            current = 1 - current;
            if (step == 0 || step == steps || step % steps_per_buffer == 0) {
                [encoder endEncoding];
                [buffer commit];
                [submitted addObject:buffer];
                encoder = nil;
            }
        }
        [buffer waitUntilCompleted];
        double time = nbody_seconds() - t0;
        for (id<MTLCommandBuffer> command in submitted) {
            if (command.error) fail(@"the kernel failed", command.error);
        }

        double e1, checksum;
        energy_and_checksum(n, pos[current].contents, vel.contents, &e1,
            &checksum);
        nbody_report(n, steps, time, e0, e1, checksum);
        printf("tile = %d  bodies_per_thread = %d  threadgroup_memory = %d"
            "  unroll = %d\n", tile, bodies_per_thread, threadgroup_memory,
            unroll);
    }
    return 0;
}
