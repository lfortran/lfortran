program arrays_op_14
implicit none
    integer :: y(3)
    real :: x(3), fvec(15)
    x = 1
    y = 2.0

    fvec = x(1)
    print *, fvec
    call verify(fvec, 1.0)

    fvec = y(2)
    print *, fvec
    call verify(fvec, 2.0)

contains

    subroutine verify(array, value)
        real, intent(in) :: array(:), value
        integer :: i
        do i = lbound(array, 1), ubound(array, 1)
            if (abs(array(i) - value) > 1e-6) error stop
        end do
    end subroutine
end program
