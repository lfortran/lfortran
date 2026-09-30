module realloc_lhs_fn_result_02_m
    implicit none

contains

    function mkarr_n(n) result(r)
        integer, intent(in) :: n
        integer :: r(n)
        integer :: i
        do i = 1, n
            r(i) = i
        end do
    end function mkarr_n

end module realloc_lhs_fn_result_02_m

program realloc_lhs_fn_result_02
    use realloc_lhs_fn_result_02_m, only: mkarr_n
    implicit none

    integer, allocatable :: t(:)

    ! Without --realloc-lhs-arrays the left hand side stays unallocated, and
    ! the runtime error must say so and point at the option that fixes it.
    t = mkarr_n(4)

    if (size(t) /= 4) error stop
end program realloc_lhs_fn_result_02
