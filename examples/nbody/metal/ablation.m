// Host of the force-kernel ablation (see ablation.metal and ANALYSIS.md).
// Runs nbody.f90's step loop -- a force kernel and the update kernel per
// step, on a structure of arrays -- with one of the force kernel variants.
//
// Usage: ablation n steps variant threadgroup_size unroll inv_sqrt
//   variant:  lfortran | unrolled | tiled
//   inv_sqrt: div (1 / sqrt(r2)) | rsqrt (rsqrt(r2))
//
// Build: clang -O2 -fobjc-arc -framework Metal -framework Foundation
//            -DNBODY_ABLATION_SOURCE='"path/to/ablation.metal"' ablation.m
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "../c/common.h"

struct force_args {
    int n;
    int lb;
};

struct update_args {
    float drift;
    float kick;
    int n;
    int lb;
};

static void fail(NSString *what, NSError *error)
{
    fprintf(stderr, "%s: %s\n", what.UTF8String,
        error ? error.localizedDescription.UTF8String : "");
    exit(1);
}

int main(int argc, char **argv)
{
    @autoreleasepool {
        if (argc != 7) {
            fprintf(stderr, "usage: %s n steps lfortran|unrolled|tiled "
                "threadgroup_size unroll div|rsqrt\n", argv[0]);
            return 1;
        }
        int n = atoi(argv[1]), steps = atoi(argv[2]);
        NSString *variant = @(argv[3]);
        int group_size = atoi(argv[4]), unroll = atoi(argv[5]);
        bool rsqrt = strcmp(argv[6], "rsqrt") == 0;
        if (n < 1 || steps < 1 || group_size < 1 || group_size > 1024 ||
            unroll < 1 || unroll > 16) fail(@"invalid size, steps, group or unroll", nil);
        if (n % group_size != 0) fail(@"n must be a multiple of the "
            "threadgroup size", nil);

        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) fail(@"no Metal device", nil);
        NSError *error = nil;
        NSString *source = [NSString
            stringWithContentsOfFile:@NBODY_ABLATION_SOURCE
                            encoding:NSUTF8StringEncoding
                               error:&error];
        if (!source) fail(@"cannot read " @NBODY_ABLATION_SOURCE, error);
        MTLCompileOptions *options = [MTLCompileOptions new];
        options.preprocessorMacros = @{
            @"TILE" : @(group_size),
            @"UNROLL" : @(unroll),
            @"USE_RSQRT" : @(rsqrt ? 1 : 0),
        };
        id<MTLLibrary> library = [device newLibraryWithSource:source
                                                      options:options
                                                        error:&error];
        if (!library) fail(@"cannot compile the kernels", error);
        id<MTLComputePipelineState> force = [device
            newComputePipelineStateWithFunction:
                [library newFunctionWithName:variant]
                                          error:&error];
        id<MTLComputePipelineState> update = [device
            newComputePipelineStateWithFunction:
                [library newFunctionWithName:@"update"]
                                          error:&error];
        if (!force || !update) fail(@"cannot create the pipelines", error);
        if ((NSUInteger)group_size > force.maxTotalThreadsPerThreadgroup ||
            (NSUInteger)group_size > update.maxTotalThreadsPerThreadgroup)
            fail(@"threadgroup exceeds pipeline limit", nil);

        // x, y, z, vx, vy, vz, m, ax, ay, az
        id<MTLBuffer> b[10];
        for (int k = 0; k < 10; k++) {
            b[k] = [device newBufferWithLength:sizeof(float) * n
                                       options:MTLResourceStorageModeShared];
            if (!b[k]) fail(@"cannot allocate device buffer", nil);
        }
        float *x = b[0].contents, *y = b[1].contents, *z = b[2].contents,
            *vx = b[3].contents, *vy = b[4].contents, *vz = b[5].contents,
            *m = b[6].contents;
        nbody_init(n, x, y, z, vx, vy, vz, m);
        double e0 = nbody_energy(n, x, y, z, vx, vy, vz, m);

        id<MTLCommandQueue> queue = [device newCommandQueue];
        if (!queue) fail(@"cannot create command queue", nil);
        MTLSize grid = MTLSizeMake(n, 1, 1);
        MTLSize group = MTLSizeMake(group_size, 1, 1);
        id<MTLBuffer> force_buffers[7] = {b[7], b[8], b[9], b[6], b[0], b[1],
            b[2]};
        id<MTLBuffer> update_buffers[9] = {b[7], b[8], b[9], b[3], b[4], b[5],
            b[0], b[1], b[2]};
        id<MTLCommandBuffer> buffer = nil;
        NSMutableArray<id<MTLCommandBuffer>> *submitted = [NSMutableArray new];
        double t0 = 0;
        for (int step = 0; step <= steps; step++) {
            if (step == 1) {
                [buffer waitUntilCompleted];
                if (buffer.error) fail(@"initial step failed", buffer.error);
                t0 = nbody_seconds();
            }
            buffer = [queue commandBuffer];
            id<MTLComputeCommandEncoder> encoder =
                [buffer computeCommandEncoder];
            struct force_args fa = {n, 1};
            [encoder setComputePipelineState:force];
            for (int k = 0; k < 7; k++) {
                [encoder setBuffer:force_buffers[k] offset:0 atIndex:k];
            }
            [encoder setBytes:&fa length:sizeof(fa) atIndex:7];
            [encoder dispatchThreads:grid threadsPerThreadgroup:group];
            struct update_args ua = {step == steps ? 0 : nbody_dt,
                (step == 0 || step == steps) ? nbody_dt / 2 : nbody_dt, n, 1};
            [encoder setComputePipelineState:update];
            for (int k = 0; k < 9; k++) {
                [encoder setBuffer:update_buffers[k] offset:0 atIndex:k];
            }
            [encoder setBytes:&ua length:sizeof(ua) atIndex:9];
            [encoder dispatchThreads:grid threadsPerThreadgroup:group];
            [encoder endEncoding];
            [buffer commit];
            [submitted addObject:buffer];
        }
        [buffer waitUntilCompleted];
        double time = nbody_seconds() - t0;
        for (id<MTLCommandBuffer> command in submitted) {
            if (command.error) fail(@"the kernel failed", command.error);
        }
        double e1 = nbody_energy(n, x, y, z, vx, vy, vz, m);
        nbody_report(n, steps, time, e0, e1, nbody_checksum(n, x, y, z));
        printf("variant = %s  threadgroup = %d  unroll = %d  inv_sqrt = %s\n",
            argv[3], group_size, unroll, argv[6]);
    }
    return 0;
}
