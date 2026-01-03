subroutine chkder(m, Err)
    implicit none

    integer, intent(in) :: m
    real, intent(out) :: Err(m)
    real :: zero
    zero = 0.0

    Err = zero

end subroutine chkder

program main
    implicit none

    integer :: i
    real :: Err(2)
    call chkder(2, Err)
    do i = 1, 2
        if (Err(i) /= 0.0) error stop
    end do
end program
