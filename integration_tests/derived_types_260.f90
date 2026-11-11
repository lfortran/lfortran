program derived_types_260
    use derived_types_260_mod
    implicit none
    type :: qm2
        integer :: n = 0
        real :: x = 1.5
        character(len=3) :: c = "dft"
    end type
    type(qm2), parameter :: loc(2) = [qm2(6, 2.5, "abc"), qm2(8, 3.5, "xyz")]
    type(qm2), parameter :: s = qm2()
    integer, parameter :: k = s%n
    integer :: i

    if (k /= 0) error stop 1
    if (s%x /= 1.5) error stop 2
    if (s%c /= "dft") error stop 3
    if (loc(2)%n /= 8) error stop 4
    if (set(1)%x /= 2.5) error stop 5

    do i = 1, 2
        if (loc(i)%n /= 6 + 2*(i-1)) error stop 6
        if (loc(i)%x /= 2.5 + (i-1)) error stop 7
        if (loc(i)%c /= merge("abc", "xyz", i == 1)) error stop 8
        if (set(i)%n /= 6 + 2*(i-1)) error stop 9
        if (set(i)%x /= 2.5 + (i-1)) error stop 10
        if (set(i)%c /= merge("abc", "xyz", i == 1)) error stop 11
    end do
end program
