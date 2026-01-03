module template_lapack_01_m
    implicit none
    private
    public :: test_template

    requirement gemm_r(T, gemm)
        type, deferred :: T
        subroutine gemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            character, intent(in) :: transa, transb
            integer, intent(in) :: m, n, k, lda, ldb, ldc
            type(T), intent(in) :: alpha, a(lda, *), b(ldb, *), beta
            type(T), intent(out) :: c(ldc, *)
        end subroutine
    end requirement

    requirement cast_r(T, U, cast)
        type, deferred :: T
        type, deferred :: U
        pure elemental function cast(arg) result(res)
            type(T), intent(in) :: arg
            type(U) :: res
        end function
    end requirement

    template external_matmul_t(T, gemm, cast_to_T)
        require :: gemm_r(T, gemm)
        require :: cast_r(real, T, cast_to_T)
        private
    contains
        function nonsimple_external_matmul(a,b) result(c)
            type(T), intent(in) :: a(:,:), b(:,:)
            type(T) :: c(size(a,1), size(b,2))
            integer :: m, n, k
            m = size(a, dim=1)
            n = size(b, dim=2)
            k = size(a, dim=1)
            call gemm('n', 'n', m, n, k, cast_to_T(1.0), a, m, b, k, cast_to_T(0.0), c, m)
        end function
    end template

contains

    subroutine my_gemm_real(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
        character, intent(in) :: transa, transb
        integer, intent(in) :: m, n, k, lda, ldb, ldc
        real, intent(in) :: alpha, a(lda, *), b(ldb, *), beta
        real, intent(out) :: c(ldc, *)
    end subroutine

    subroutine my_gemm_double(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
        integer, parameter :: dp = kind(1.d0)
        character, intent(in) :: transa, transb
        integer, intent(in) :: m, n, k, lda, ldb, ldc
        real(dp), intent(in) :: alpha, a(lda, *), b(ldb, *), beta
        real(dp), intent(out) :: c(ldc, *)
    end subroutine

    pure elemental function my_cast_to_real(a) result(b)
        real, intent(in) :: a
        real :: b
        b = a
    end function

    pure elemental function my_cast_to_double(a) result(b)
        integer, parameter :: dp = kind(1.d0)
        real, intent(in) :: a
        real(dp) :: b
        b = a
    end function

    function my_external_matmul(a, b) result(c)
        real, intent(in) :: a(:,:), b(:,:)
        real :: c(size(a,1), size(b,2))
        integer :: m, n, k
        m = size(a, dim=1)
        n = size(b, dim=2)
        k = size(a, dim=1)
        call my_gemm_real('n', 'n', m, n, k, my_cast_to_real(1.0), a, m, b, k, my_cast_to_real(0.0), c, m)
    end function

    function simple_external_matmul {T, gemm, cast_to_T} (a, b) result(c)
        require :: gemm_r(T, gemm)
        require :: cast_r(real, T, cast_to_T)
        type(T), intent(in) :: a(:,:), b(:,:)
        type(T) :: c(size(a,1), size(b,2))
        integer :: m, n, k
        m = size(a, dim=1)
        n = size(b, dim=2)
        k = size(a, dim=1)
        call gemm('n', 'n', m, n, k, cast_to_T(1.0), a, m, b, k, cast_to_T(0.0), c, m)
    end function

    subroutine test_template()
        integer, parameter :: dp = kind(1.d0)
        instantiate external_matmul_t(real, my_gemm_real, my_cast_to_real), &
            only: nonsimple_external_matmul_real => nonsimple_external_matmul
        instantiate external_matmul_t(real(dp), my_gemm_double, my_cast_to_double), &
            only: nonsimple_external_matmul_double => nonsimple_external_matmul
        
        real :: asp(2,2), bsp(2,2), csp(2,2)
        real(dp) :: adp(2,2), bdp(2,2), cdp(2,2)

        csp = simple_external_matmul {real, my_gemm_real, my_cast_to_real} (asp, bsp)
        cdp = simple_external_matmul {real(dp), my_gemm_double, my_cast_to_double} (adp, bdp)
    end subroutine

end module

program template_lapack_01
use template_lapack_01_m
implicit none

call test_template()

end program