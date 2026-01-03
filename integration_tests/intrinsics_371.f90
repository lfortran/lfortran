program intrinsics_371
    implicit none

    type :: stone
        integer :: value = 0 
    end type stone

    type(stone),dimension(:),allocatable :: s
    integer :: val , iloc

    allocate(s(0))
    val = 1

    iloc = findloc(s%value, val, dim=1)

    print * , iloc
    if (iloc /= 0) error stop
end program