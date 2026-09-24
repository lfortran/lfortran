module realloc_lhs_fn_result_03_m
    implicit none

    type :: item
        integer, allocatable :: v(:)
    end type item

    abstract interface
        function mkarr_2d_i(n) result(r)
            import :: item
            integer, intent(in) :: n
            type(item) :: r(n, 2)
        end function mkarr_2d_i
    end interface

    type :: holder
        procedure(mkarr_2d_i), pointer, nopass :: p => null()
    end type holder

contains

    function mkarr_2d(n) result(r)
        integer, intent(in) :: n
        type(item) :: r(n, 2)
        integer :: i, j
        do j = 1, 2
            do i = 1, n
                allocate(r(i, j)%v(i))
                r(i, j)%v = i*j
            end do
        end do
    end function mkarr_2d

end module realloc_lhs_fn_result_03_m

program realloc_lhs_fn_result_03
    use realloc_lhs_fn_result_03_m, only: item, holder, mkarr_2d
    implicit none

    type(holder) :: h
    type(item), allocatable :: u(:, :)

    h%p => mkarr_2d

    ! The result comes back through a procedure pointer component, but the
    ! left hand side is still unallocated, so the same runtime error must be
    ! reported as for a direct call.
    u = h%p(3)

    if (size(u, 1) /= 3) error stop
end program realloc_lhs_fn_result_03
