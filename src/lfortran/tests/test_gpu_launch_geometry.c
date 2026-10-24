#include "lfortran_gpu_runtime.h"

#if defined(__OBJC__)
static void register_kernels(void)
{
    lfortran_gpu_register_metal_source("geometry_write",
        "#include <metal_stdlib>\n"
        "using namespace metal;\n"
        "kernel void geometry_write(device int *out [[buffer(0)]])"
        " { out[0] = 7; }\n");
    lfortran_gpu_register_metal_source("geometry_empty",
        "#include <metal_stdlib>\n"
        "using namespace metal;\n"
        "kernel void geometry_empty() {}\n");
}
#else
// As in the code the CUDA code generator emits for the CPU emulation, the
// kernels are thunks, registered with the type of a kernel.
typedef void (*kernel_func_t)(void);
void lfortran_gpu_register_kernel(const char *name, kernel_func_t func);

static void geometry_write(void **args)
{
    int *out = *(int **)args[0];
    *out = 7;
}

static void geometry_empty(void **args)
{
    (void)args;
}

static void register_kernels(void)
{
    lfortran_gpu_register_kernel("geometry_write",
        (kernel_func_t)geometry_write);
    lfortran_gpu_register_kernel("geometry_empty",
        (kernel_func_t)geometry_empty);
}
#endif

int main(int argc, char **argv)
{
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    int empty_body = argc == 2 && strcmp(argv[1], "empty-body") == 0;
    if (!empty_body && argc != 7) return 2;
    int positive = 1;
    if (!empty_body) {
        for (int i = 0; i < 3; i++) {
            grid[i] = atoi(argv[i + 1]);
            block[i] = atoi(argv[i + 4]);
            positive &= grid[i] > 0 && block[i] > 0;
        }
    }
    register_kernels();
    lfortran_gpu_ctx *ctx = lfortran_gpu_init();
    if (!ctx) return 2;
    const char *name = empty_body ? "geometry_empty" : "geometry_write";
    lfortran_gpu_kernel *kernel = lfortran_gpu_load_kernel(ctx, name,
        (int)strlen(name));
    if (!kernel) return 2;
    int out = 0;
    if (!empty_body) {
        lfortran_gpu_set_buffer_arg(kernel, 0, &out, sizeof(out),
            LFORTRAN_GPU_WRITE);
    }
    lfortran_gpu_launch(ctx, kernel, grid, block);
    lfortran_gpu_sync(ctx);
    lfortran_gpu_release_kernel(kernel);
    lfortran_gpu_shutdown(ctx);
    if (!empty_body && positive && out != 7) return 2;
    printf("launch returned: %d\n", out);
    return 0;
}
