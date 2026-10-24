module allocate_77_m
    implicit none
    type :: point
        integer :: x
    end type
end module

program allocate_77
    ! Assignment to an unallocated allocatable derived-type scalar
    ! allocates it, also with the C backend (#15949)
    use allocate_77_m
    implicit none
    type(point), allocatable :: a
    type(point) :: b
    b%x = 42
    a = b
    if (a%x /= 42) error stop 1
    b%x = 7
    a = b
    if (a%x /= 7) error stop 2
    print *, a%x
end program
