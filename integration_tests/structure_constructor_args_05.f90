! The identity `reduce` starts from for a derived type gives every component
! its own shape and length: array components get a zero array, character
! components a blank string of their length, and allocatable components start
! unallocated.
module structure_constructor_args_05_m
    implicit none
    type :: t
        integer :: n
        integer :: a(2)
        real :: r(2, 3)
        character(len=3) :: c
        character(len=2) :: ca(2)
        integer, allocatable :: al(:)
    end type
contains
    pure function add(x, y) result(r)
        type(t), intent(in) :: x, y
        type(t) :: r
        r%n = x%n + y%n
        r%a = x%a + y%a
        r%r = x%r + y%r
        r%c = y%c
        r%ca = y%ca
    end function
end module

program structure_constructor_args_05
    use structure_constructor_args_05_m
    implicit none
    type(t) :: arr(3), s
    integer :: i
    do i = 1, 3
        arr(i)%n = i
        arr(i)%a = i
        arr(i)%r = i
        arr(i)%c = "abc"
        arr(i)%ca = "xy"
    end do
    s = reduce(arr, add)
    print *, s%n, s%a, s%r, s%c, s%ca
    if (s%n /= 6) error stop 1
    if (any(s%a /= 6)) error stop 2
    if (any(s%r /= 6.0)) error stop 3
    if (s%c /= "abc") error stop 4
    if (any(s%ca /= "xy")) error stop 5
end program
