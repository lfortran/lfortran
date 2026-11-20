! spacing(x) is the spacing of the model numbers at |x| and not below tiny:
! negative arguments, powers of two, huge, tiny and zero.
program spacing_range_01
    implicit none
    real(8) :: x(6), s(6)
    real(4) :: y
    integer :: i
    x = [-1.0d0, 2.0d0, -2.0d0, huge(1d0), tiny(1d0), 0.0d0]
    s = [epsilon(1d0), 2*epsilon(1d0), 2*epsilon(1d0), 2d0**971, tiny(1d0), tiny(1d0)]
    do i = 1, size(x)
        print *, x(i), spacing(x(i))
        if (spacing(x(i)) /= s(i)) error stop "real(8)"
    end do
    y = -0.5
    if (spacing(y) /= epsilon(1.0)/2) error stop "real(4)"
end program
