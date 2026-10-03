module realloc_lhs_fn_result_01_m
    implicit none

    type :: item
        integer, allocatable :: v(:)
    end type item

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

end module realloc_lhs_fn_result_01_m

program realloc_lhs_fn_result_01
    use realloc_lhs_fn_result_01_m, only: item, mkarr_2d
    implicit none

    type(item), allocatable :: u(:, :)

    ! Without --realloc-lhs-arrays the left hand side stays unallocated, and
    ! the runtime error must say so and point at the option that fixes it.
    u = mkarr_2d(3)

    if (size(u, 1) /= 3) error stop
end program realloc_lhs_fn_result_01
