/* CPU peak microbenchmarks for Apple Silicon (AArch64).
 *
 * 1. Clock: a chain of dependent integer adds (one cycle each) gives the
 *    cycles per second of the core running it.
 * 2. FP32 peak: 20 independent chains of 128-bit fused multiply-adds
 *    (fmla v.4s), enough to cover the latency of every FP/SIMD pipe, give
 *    the FMA throughput. Each fmla.4s is 4 lanes x 2 flops = 8 flops.
 *
 * Both are run on 1 thread, on the performance cores (12 threads, QoS
 * user-interactive) and on the efficiency cores (4 threads, QoS background,
 * which macOS runs on the efficiency cores only).
 *
 * Build: clang -O2 fma_cpu.c -o fma_cpu */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/qos.h>
#include <time.h>

static double seconds(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + 1e-9 * t.tv_nsec;
}

/* 64 dependent adds per iteration. */
static void add_chain(long iters)
{
    long x = 0;
    __asm__ volatile(
        "1:\n"
        ".rept 64\n"
        "add %1, %1, #1\n"
        ".endr\n"
        "subs %0, %0, #1\n"
        "b.ne 1b\n"
        : "+r"(iters), "+r"(x)
        :
        : "cc");
}
static const double add_chain_cycles = 64;

/* 120 fmla.4s per iteration over 20 independent accumulators: the 4-cycle
 * latency of the 4 FP/SIMD pipes needs at least 16; 20 measured best (with
 * 24 or more the throughput drops to about 3 per cycle). */
static void fma_chains(long iters)
{
    __asm__ volatile(
        "movi v30.4s, #0\n"
        "movi v31.4s, #0\n"
        "movi v0.4s, #0\n"
        "movi v1.4s, #0\n"
        "movi v2.4s, #0\n"
        "movi v3.4s, #0\n"
        "movi v4.4s, #0\n"
        "movi v5.4s, #0\n"
        "movi v6.4s, #0\n"
        "movi v7.4s, #0\n"
        "movi v8.4s, #0\n"
        "movi v9.4s, #0\n"
        "movi v10.4s, #0\n"
        "movi v11.4s, #0\n"
        "movi v12.4s, #0\n"
        "movi v13.4s, #0\n"
        "movi v14.4s, #0\n"
        "movi v15.4s, #0\n"
        "movi v16.4s, #0\n"
        "movi v17.4s, #0\n"
        "movi v18.4s, #0\n"
        "movi v19.4s, #0\n"
        "1:\n"
        ".rept 6\n"
        "fmla v0.4s, v30.4s, v31.4s\n"
        "fmla v1.4s, v30.4s, v31.4s\n"
        "fmla v2.4s, v30.4s, v31.4s\n"
        "fmla v3.4s, v30.4s, v31.4s\n"
        "fmla v4.4s, v30.4s, v31.4s\n"
        "fmla v5.4s, v30.4s, v31.4s\n"
        "fmla v6.4s, v30.4s, v31.4s\n"
        "fmla v7.4s, v30.4s, v31.4s\n"
        "fmla v8.4s, v30.4s, v31.4s\n"
        "fmla v9.4s, v30.4s, v31.4s\n"
        "fmla v10.4s, v30.4s, v31.4s\n"
        "fmla v11.4s, v30.4s, v31.4s\n"
        "fmla v12.4s, v30.4s, v31.4s\n"
        "fmla v13.4s, v30.4s, v31.4s\n"
        "fmla v14.4s, v30.4s, v31.4s\n"
        "fmla v15.4s, v30.4s, v31.4s\n"
        "fmla v16.4s, v30.4s, v31.4s\n"
        "fmla v17.4s, v30.4s, v31.4s\n"
        "fmla v18.4s, v30.4s, v31.4s\n"
        "fmla v19.4s, v30.4s, v31.4s\n"
        ".endr\n"
        "subs %0, %0, #1\n"
        "b.ne 1b\n"
        : "+r"(iters)
        :
        : "cc", "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8",
          "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16",
          "v17", "v18", "v19", "v30", "v31");
}
static const double fma_chains_flops = 120 * 8;

typedef struct {
    void (*kernel)(long);
    long iters;
    qos_class_t qos;
    double time;
} job_t;

static void *run_job(void *arg)
{
    job_t *job = arg;
    pthread_set_qos_class_self_np(job->qos, 0);
    job->kernel(job->iters / 10);  /* warm up, let the clock ramp up */
    double t = seconds();
    job->kernel(job->iters);
    job->time = seconds() - t;
    return NULL;
}

/* Runs the kernel on `threads` threads at once and returns the work done per
 * second by each thread, averaged over the threads. */
static double rate_per_thread(void (*kernel)(long), long iters, double work,
    int threads, qos_class_t qos)
{
    pthread_t tid[64];
    job_t job[64];
    for (int t = 0; t < threads; t++) {
        job[t] = (job_t){kernel, iters, qos, 0};
        int status = pthread_create(&tid[t], NULL, run_job, &job[t]);
        if (status) {
            fprintf(stderr, "pthread_create: %s\n", strerror(status));
            exit(1);
        }
    }
    double rate = 0;
    for (int t = 0; t < threads; t++) {
        pthread_join(tid[t], NULL);
        rate += iters * work / job[t].time;
    }
    return rate / threads;
}

static void measure(const char *name, int threads, qos_class_t qos)
{
    double ghz = rate_per_thread(add_chain, 40000000, add_chain_cycles,
        threads, qos) / 1e9;
    double gflops = rate_per_thread(fma_chains, 4000000, fma_chains_flops,
        threads, qos) / 1e9;
    printf("%-30s %2d threads: clock %5.2f GHz, %6.1f GFLOP/s per core "
        "(%4.1f flops/cycle), %7.1f GFLOP/s total\n", name, threads, ghz,
        gflops, gflops / ghz, gflops * threads);
}

int main(int argc, char **argv)
{
    int p_cores = argc >= 2 ? atoi(argv[1]) : 12;
    int e_cores = argc >= 3 ? atoi(argv[2]) : 4;
    if (p_cores < 1 || p_cores > 64 || e_cores < 1 || e_cores > 64) {
        fprintf(stderr, "core counts must be in 1..64\n");
        return 1;
    }
    measure("1 performance core", 1, QOS_CLASS_USER_INTERACTIVE);
    measure("all performance cores", p_cores, QOS_CLASS_USER_INTERACTIVE);
    measure("efficiency cores", e_cores, QOS_CLASS_BACKGROUND);
    return 0;
}
