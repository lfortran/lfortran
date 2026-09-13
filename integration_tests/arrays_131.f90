program arrays_131
implicit none
call automatic_array(4)

contains

    ! `a` is an automatic array: its extent is only known at runtime.  The C++
    ! backend encodes the extents of an array into the name of the struct it
    ! generates for it, and a symbolic extent contributes no digits at all.
    subroutine automatic_array(n)
    integer, intent(in) :: n
    integer :: a(n)
    integer :: i, s
    do i = 1, n
        a(i) = i
    end do
    s = 0
    do i = 1, n
        s = s + a(i)
    end do
    if (s /= 10) error stop
    end subroutine

end program
