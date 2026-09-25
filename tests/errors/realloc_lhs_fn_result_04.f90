module realloc_lhs_fn_result_04_m
    implicit none

    abstract interface
        function mkarr_n_i(n) result(r)
            integer, intent(in) :: n
            integer :: r(n)
        end function mkarr_n_i
    end interface

contains

    function mkarr_n(n) result(r)
        integer, intent(in) :: n
        integer :: r(n)
        integer :: i
        do i = 1, n
            r(i) = i
        end do
    end function mkarr_n

    subroutine use_it(f)
        procedure(mkarr_n_i) :: f
        integer, allocatable :: t(:)
        ! The result comes back through a dummy procedure, but the left hand
        ! side is still unallocated, so the same runtime error must be
        ! reported as for a direct call.
        t = f(4)
        if (size(t) /= 4) error stop
    end subroutine use_it

end module realloc_lhs_fn_result_04_m

program realloc_lhs_fn_result_04
    use realloc_lhs_fn_result_04_m, only: mkarr_n, use_it
    implicit none

    call use_it(mkarr_n)
end program realloc_lhs_fn_result_04
