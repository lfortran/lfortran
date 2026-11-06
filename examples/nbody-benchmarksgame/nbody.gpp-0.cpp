
/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Miles for C
 * Ported to C++ with std::array and compile-time loops by François-David Collin
 */
#include <cstdio>
#include <array>
#include <immintrin.h>

constexpr double PI = 3.141592653589793;
constexpr double SOLAR_MASS = 4 * PI * PI;
constexpr double DAYS_PER_YEAR = 365.24;

constexpr size_t N = 5;

// Vector Code
using Vf = __m256d;

// utilize vrsqrtps to compute an approximation of 1/sqrt(x) with float,
// cast back to double and refine using a variation of
// Goldschmidt’s algorithm to get < 1e-9 error
static inline __m256d _mm256_rsqrt_pd(__m256d s)
{
    __m128 q = _mm256_cvtpd_ps(s);
    q = _mm_rsqrt_ps(q);
    __m256d x = _mm256_cvtps_pd(q);
    __m256d y = s * x * x;
    __m256d a = _mm256_mul_pd(y, _mm256_set1_pd(0.375));
    a = _mm256_mul_pd(a, y);
    __m256d b = _mm256_mul_pd(y, _mm256_set1_pd(1.25));
    b = _mm256_sub_pd(b, _mm256_set1_pd(1.875));
    y = _mm256_sub_pd(a, b);
    x = _mm256_mul_pd(x, y);
    return x;
}

template<size_t n, size_t pairs = n*(n-1)/2>
constexpr std::array<std::array<size_t,2>,pairs> getpairs() {
    std::array<std::array<size_t,2>,pairs> listpairs{};
    for(size_t i = 0, k = 0; i < n; i++) 
        for (size_t j = 0; j < i; j++, k++)
            listpairs[k] = std::array<size_t,2>{i,j};
    return listpairs;
}

constexpr auto listpairs = getpairs<N>();
constexpr size_t PAIRS = listpairs.size();

struct NBodySystem {
    std::array<Vf,N> p;
    std::array<Vf,N> v;
    std::array<Vf,N> m;
    const __m256d rt;

    NBodySystem(double dt) : rt(_mm256_set1_pd(dt)) {

        r[PAIRS] = _mm256_set1_pd(1.0);
        r[PAIRS+1] = _mm256_set1_pd(1.0);
        r[PAIRS+2] = _mm256_set1_pd(1.0);

        // sun
        m[0] = _mm256_set1_pd(SOLAR_MASS);
        p[0] = _mm256_set1_pd(0.0);
        v[0] = _mm256_set1_pd(0.0);

        // jupiter
        m[1] = _mm256_set1_pd(9.54791938424326609e-04 * SOLAR_MASS);
        p[1] = _mm256_setr_pd(0.0,
                            4.84143144246472090e+00,
                            -1.16032004402742839e+00,
                            -1.03622044471123109e-01);
        v[1] = _mm256_setr_pd(0.0,
                            1.66007664274403694e-03 * DAYS_PER_YEAR,
                            7.69901118419740425e-03 * DAYS_PER_YEAR,
                            -6.90460016972063023e-05 * DAYS_PER_YEAR);

        // saturn
        m[2] = _mm256_set1_pd(2.85885980666130812e-04 * SOLAR_MASS);
        p[2] = _mm256_setr_pd(0.0,
                            8.34336671824457987e+00,
                            4.12479856412430479e+00,
                            -4.03523417114321381e-01);
        v[2] = _mm256_setr_pd(0.0,
                            -2.76742510726862411e-03 * DAYS_PER_YEAR,
                            4.99852801234917238e-03 * DAYS_PER_YEAR,
                            2.30417297573763929e-05 * DAYS_PER_YEAR);

        // uranus
        m[3] = _mm256_set1_pd(4.36624404335156298e-05 * SOLAR_MASS);
        p[3] = _mm256_setr_pd(0.0,
                            1.28943695621391310e+01,
                            -1.51111514016986312e+01,
                            -2.23307578892655734e-01);
        v[3] = _mm256_setr_pd(0.0,
                            2.96460137564761618e-03 * DAYS_PER_YEAR,
                            2.37847173959480950e-03 * DAYS_PER_YEAR,
                            -2.96589568540237556e-05 * DAYS_PER_YEAR);

        // neptune
        m[4] = _mm256_set1_pd(5.15138902046611451e-05 * SOLAR_MASS);
        p[4] = _mm256_setr_pd(0.0,
                            1.53796971148509165e+01,
                            -2.59193146099879641e+01,
                            1.79258772950371181e-01);
        v[4] = _mm256_setr_pd(0.0,
                            2.68067772490389322e-03 * DAYS_PER_YEAR,
                            1.62824170038242295e-03 * DAYS_PER_YEAR,
                            -9.51592254519715870e-05 * DAYS_PER_YEAR);

        __m256d o = _mm256_set1_pd(0.0);
        for (int i = 0; i < N; i++)
        {
            __m256d t = _mm256_mul_pd(m[i], v[i]);
            o = _mm256_add_pd(o, t);
        }
        v[0] = _mm256_mul_pd(o, _mm256_set1_pd(-1.0 / SOLAR_MASS));
    }

    std::array<Vf,PAIRS+3> r;
    alignas(32) std::array<double,PAIRS+3> w;


    template<size_t k = 0> 
    inline void KernelLoop0() {
        constexpr auto i = listpairs[k][0];
        constexpr auto j = listpairs[k][1];
        r[k] = _mm256_sub_pd(p[i], p[j]);
        if constexpr ((k + 1) < PAIRS) KernelLoop0<k+1>();
    }

    template<size_t k = 0>
    inline void KernelLoop1() {
        __m256d x0 = _mm256_mul_pd(r[k], r[k]);
        __m256d x1 = _mm256_mul_pd(r[k + 1], r[k + 1]);
        __m256d x2 = _mm256_mul_pd(r[k + 2], r[k + 2]);
        __m256d x3 = _mm256_mul_pd(r[k + 3], r[k + 3]);

        __m256d t0 = _mm256_hadd_pd(x0, x1);
        __m256d t1 = _mm256_hadd_pd(x2, x3);
        __m256d y0 = _mm256_permute2f128_pd(t0, t1, 0x21);
        __m256d y1 = _mm256_blend_pd(t0, t1, 0b1100);

        __m256d z = _mm256_add_pd(y0, y1);
        z = _mm256_rsqrt_pd(z);
        _mm256_store_pd(&w[k], z);
        if constexpr ((k + 4) < PAIRS) KernelLoop1<k+4>();
    }

    double energy() {
        double e = 0.0;

        r[N] = _mm256_set1_pd(0.0);
        r[N+1] = _mm256_set1_pd(0.0);
        r[N+2] = _mm256_set1_pd(0.0);

        for (int k = 0; k < N; k++)
            r[k] = _mm256_mul_pd(v[k], v[k]);

        for (int k = 0; k < N; k += 4) {
            __m256d t0 = _mm256_hadd_pd(r[k  ], r[k+1]);
            __m256d t1 = _mm256_hadd_pd(r[k+2], r[k+3]);
            __m256d y0 = _mm256_permute2f128_pd(t0, t1, 0x21);
            __m256d y1 = _mm256_blend_pd(t0, t1, 0b1100);

            __m256d z = _mm256_add_pd(y0, y1);
            _mm256_store_pd(&w[k], z);
        }

        for (int k = 0; k < N; k++)
            e += 0.5 * _mm256_cvtsd_f64(m[k]) * w[k];

        KernelLoop0();
        KernelLoop1();

        for (int i = 1, k = 0; i < N; i++)
            for (int j = 0; j < i; j++, k++)
                e -= _mm256_cvtsd_f64(m[i]) * _mm256_cvtsd_f64(m[j]) * w[k];

        return e;
    }

    template<size_t k = 0>
    inline void AdvanceLoopInner0() {
        __m256d x = _mm256_load_pd(&w[k]);
        __m256d y = _mm256_mul_pd(x, x);
        __m256d z = _mm256_mul_pd(x, rt);
        x = _mm256_mul_pd(y, z);
        _mm256_store_pd(&w[k], x);
        if constexpr ((k + 4) < PAIRS) AdvanceLoopInner0<k+4>();
    }

    template<size_t k = 0>
    inline void AdvanceLoopInner1() {
        constexpr size_t i = listpairs[k][0];
        constexpr size_t j = listpairs[k][1];
        __m256d t = _mm256_set1_pd(w[k]);
        t = _mm256_mul_pd(r[k], t);
        __m256d x = _mm256_mul_pd(t, m[j]);
        __m256d y = _mm256_mul_pd(t, m[i]);

        v[i] = _mm256_sub_pd(v[i], x);
        v[j] = _mm256_add_pd(v[j], y);

        if constexpr ((k + 1) < PAIRS) AdvanceLoopInner1<k+1>();
    }


    template<size_t i = 0>
    inline void AdvanceLoopInner2() {
        __m256d t = _mm256_mul_pd(v[i], rt);
        p[i] = _mm256_add_pd(p[i], t);

        if constexpr ((i + 1) < N) AdvanceLoopInner2<i+1>();
    }


    inline void advance() {
        // compute rsqrt of distance between each pair of bodies
        KernelLoop0();
        KernelLoop1();

        AdvanceLoopInner0();
        AdvanceLoopInner1();
        AdvanceLoopInner2();
    }
};

int main(int argc, char *argv[]) {
    int n = atoi(argv[1]);
    NBodySystem bodies(0.01);
    printf("%.9f\n", bodies.energy());
    for (int i=0; i < n; ++i) bodies.advance();
    printf("%.9f\n", bodies.energy());
}
    