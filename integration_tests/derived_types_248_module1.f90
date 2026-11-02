module derived_types_248_module1
    implicit none
    type :: node
        integer :: a = 1
        integer, allocatable :: d(:)
    end type
    type :: holder1
        type(node), allocatable :: arr(:)
    end type
contains
    subroutine test1()
        type(holder1) :: x, y
        allocate(x%arr(2))
        allocate(x%arr(2)%d(3))
        x%arr(2)%d = [1, 2, 3]
        x%arr(2)%a = 11
        y = x
        if (size(y%arr) /= 2) error stop 10
        if (y%arr(2)%a /= 11) error stop 11
        if (any(y%arr(2)%d /= [1, 2, 3])) error stop 12
        y%arr(2)%d(1) = 0
        if (x%arr(2)%d(1) /= 1) error stop 13
        print *, "module1 ok"
    end subroutine
end module
