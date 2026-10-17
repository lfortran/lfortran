// GPU FP32 peak microbenchmark for Apple GPUs (Objective-C++ host, Metal
// kernels compiled at run time from source, so no Xcode is needed).
//
// fma_peak: every thread runs 16 independent chains of fused multiply-adds,
// each worth 2 flops; the grid has far more threads than the GPU runs at
// once. rsqrt_rate: the same with rsqrt instead of fma, to measure the cost
// of the reciprocal square root that every N-body interaction needs.
// The kernel time is taken from the GPU timestamps of the command buffer,
// best of several runs.
//
// Build: clang++ -O2 -fobjc-arc -framework Metal -framework Foundation fma_metal.mm
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

static NSString *const source = @R"metal(
#include <metal_stdlib>
using namespace metal;

#define CHAINS(OP) \
    OP(0) OP(1) OP(2) OP(3) OP(4) OP(5) OP(6) OP(7) \
    OP(8) OP(9) OP(10) OP(11) OP(12) OP(13) OP(14) OP(15)

kernel void fma_peak(device float *out [[buffer(0)]],
                     constant uint &iters [[buffer(1)]],
                     uint gid [[thread_position_in_grid]])
{
#define INIT(k) float a##k = gid * 1e-9f + k;
    CHAINS(INIT)
    const float b = 0.999999f, c = 1e-7f;
    for (uint i = 0; i < iters; i++) {
#define FMA(k) a##k = fma(a##k, b, c);
        CHAINS(FMA)
    }
#define SUM(k) + a##k
    out[gid] = 0 CHAINS(SUM);
}

kernel void rsqrt_rate(device float *out [[buffer(0)]],
                       constant uint &iters [[buffer(1)]],
                       uint gid [[thread_position_in_grid]])
{
    CHAINS(INIT)
    for (uint i = 0; i < iters; i++) {
#define RSQRT(k) a##k = rsqrt(a##k);
        CHAINS(RSQRT)
    }
    out[gid] = 0 CHAINS(SUM);
}
)metal";

static double run(id<MTLDevice> device, id<MTLLibrary> library,
    NSString *name, uint32_t threads, uint32_t iters)
{
    NSError *error = nil;
    id<MTLComputePipelineState> pipeline = [device
        newComputePipelineStateWithFunction:[library newFunctionWithName:name]
                                      error:&error];
    if (!pipeline) {
        fprintf(stderr, "%s\n", error.localizedDescription.UTF8String);
        exit(1);
    }
    id<MTLBuffer> out = [device newBufferWithLength:4 * threads
                                            options:MTLResourceStorageModeShared];
    id<MTLCommandQueue> queue = [device newCommandQueue];
    if (!out || !queue) {
        fprintf(stderr, "cannot allocate GPU buffer or queue\n");
        exit(1);
    }
    double best = 1e30;
    for (int rep = 0; rep < 7; rep++) {
        id<MTLCommandBuffer> buffer = [queue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [buffer computeCommandEncoder];
        [encoder setComputePipelineState:pipeline];
        [encoder setBuffer:out offset:0 atIndex:0];
        [encoder setBytes:&iters length:sizeof(iters) atIndex:1];
        [encoder dispatchThreads:MTLSizeMake(threads, 1, 1)
            threadsPerThreadgroup:MTLSizeMake(256, 1, 1)];
        [encoder endEncoding];
        [buffer commit];
        [buffer waitUntilCompleted];
        if (buffer.error) {
            fprintf(stderr, "%s\n", buffer.error.localizedDescription.UTF8String);
            exit(1);
        }
        double t = buffer.GPUEndTime - buffer.GPUStartTime;
        if (t <= 0) {
            fprintf(stderr, "invalid GPU timestamp interval\n");
            exit(1);
        }
        if (rep > 0 && t < best) best = t;  // the first run warms up
    }
    return (double)threads * iters * 16 / best;
}

int main(void)
{
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) {
            fprintf(stderr, "no Metal device\n");
            return 1;
        }
        NSError *error = nil;
        id<MTLLibrary> library = [device newLibraryWithSource:source
                                                      options:nil
                                                        error:&error];
        if (!library) {
            fprintf(stderr, "%s\n", error.localizedDescription.UTF8String);
            return 1;
        }
        const uint32_t threads = 1 << 20, iters = 8192;
        double fma = run(device, library, @"fma_peak", threads, iters);
        double rsqrt = run(device, library, @"rsqrt_rate", threads, iters);
        printf("%s\n", device.name.UTF8String);
        printf("fma:   %8.1f GFLOP/s (%.1f G fma/s)\n", 2 * fma / 1e9,
            fma / 1e9);
        printf("rsqrt: %8.1f G rsqrt/s (%.2f per fma)\n", rsqrt / 1e9,
            rsqrt / fma);
    }
    return 0;
}
