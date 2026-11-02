module derived_types_250_module1
    implicit none
contains
    subroutine work()
        type :: node
            integer :: a = 1
            integer, allocatable :: d(:)
        end type
        type :: holder
            type(node), allocatable :: arr(:)
        end type
        type(holder) :: x, y
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
        print *, "module1 work ok"
    end subroutine

    subroutine block_work()
        block
            type :: node
                integer :: a = 1
                integer, allocatable :: d(:)
            end type
            type :: holder
                type(node), allocatable :: arr(:)
            end type
            type(holder) :: x, y
            allocate(x%arr(2))
            allocate(x%arr(2)%d(3))
            x%arr(2)%d = [1, 2, 3]
            x%arr(2)%a = 11
            y = x
            if (size(y%arr) /= 2) error stop 150
            if (y%arr(2)%a /= 11) error stop 151
            if (any(y%arr(2)%d /= [1, 2, 3])) error stop 152
            y%arr(2)%d(1) = 0
            if (x%arr(2)%d(1) /= 1) error stop 153
            print *, "module1 block ok"
        end block
    end subroutine
end module
