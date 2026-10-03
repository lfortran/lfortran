program array_section_26
    ! Regression test for issue #11293:
    ! Passing an array slice with explicit lower bound (e.g. arr(0:,1)) of an
    ! array allocated with a non-default lower bound, to a subroutine whose
    ! dummy argument has an explicit lower bound (dimension(0:)), used to
    ! lose the writes — the subroutine wrote into a different memory region
    ! than the actual backing storage of the caller's array.
    implicit none
    real(8), allocatable :: arr_g(:,:)
    allocate(arr_g(0:5,5))
    arr_g = 0.0d0
    call set_array(arr_g(0:,1))
    if (abs(arr_g(1,1) - 1.0d0) > 1.0d-12) error stop
    if (abs(arr_g(2,1) - 1.0d0) > 1.0d-12) error stop
    if (abs(arr_g(0,1)) > 1.0d-12) error stop
    if (abs(arr_g(3,1)) > 1.0d-12) error stop
contains
    subroutine set_array(arr)
        real(8), dimension(0:) :: arr
        arr(1) = 1.0d0
        arr(2) = 1.0d0
        if (abs(arr(1) - 1.0d0) > 1.0d-12) error stop
    end subroutine
end program
