program arrayitem_02
    implicit none

    type :: a1
        integer, allocatable :: x(:)
    end type

    type :: a2
        type(a1) :: y(4)
    end type

    type :: a3
        type(a2) :: z(3)
    end type

    type :: a4
        type(a3) :: w(2)
    end type

    type(a4) :: obj

    allocate(obj%w(1)%z(2)%y(3)%x(50))
    print *, size(obj%w(1)%z(2)%y(3)%x)
    if(size(obj%w(1)%z(2)%y(3)%x) /= 50) error stop
end program