module stdlib_quadrature_simps
    interface simps38_weights
        module procedure simps38_weights_dp
    end interface simps38_weights

contains

    function simps38_weights_dp(x) result(w)
        real(8), intent(in) :: x(4)
        real(8) :: w(size(x))
    end function simps38_weights_dp

    subroutine simps38_weights_dp_use(x1)
        real(8), intent(in) :: x1(4)
        print *, simps38_weights(x1)
    end subroutine simps38_weights_dp_use
end module

program stdlib_quadrature
use stdlib_quadrature_simps, only: simps38_weights_dp
implicit none
real(8) :: x1(4)
print *, simps38_weights_dp(x1)
end program
