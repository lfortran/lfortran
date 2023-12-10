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

    template external_matmul_t(U, gemm, cast_to_T)
        require :: gemm_r(U, gemm)
        !require :: cast_r(real, T, cast_to_T)
        private
    end template

contains

    ! for reference
    subroutine my_gemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
        character, intent(in) :: transa, transb
        integer, intent(in) :: m, n, k, lda, ldb, ldc
        real, intent(in) :: alpha, a(lda, *), b(ldb, *), beta
        real, intent(out) :: c(ldc, *)
    end subroutine

    !function external_matmul {T, gemm, cast_to_T} (a, b) result(c)
    !    require :: gemm_r(T, gemm)
    !    require :: cast_r(real, T, cast_to_T)
    !    type(T), intent(in) :: a(:,:), b(:,:)
    !    type(T) :: c(size(a,1), size(b,2))
    !    integer :: m, n, k
    !end function

    subroutine test_template()

    end subroutine

end module

program template_lapack_01
use template_lapack_01_m
implicit none

call test_template()

end program