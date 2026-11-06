module derived_types_250_module2
    implicit none
contains
    subroutine work()
        ! Same type names as in derived_types_250_module1, different layout
        type :: node
            real(8) :: r1 = 0, r2 = 0, r3 = 0
            character(len=:), allocatable :: s
            real(8), allocatable :: e(:,:)
        end type
        type :: holder
            type(node), allocatable :: arr(:)
        end type
        type(holder) :: x, y
        allocate(x%arr(2))
        x%arr(2)%s = "hello"
        allocate(x%arr(2)%e(2,2))
        x%arr(2)%e = 4
        x%arr(2)%r3 = 7
        y = x
        if (size(y%arr) /= 2) error stop 20
        if (y%arr(2)%s /= "hello") error stop 21
        if (any(y%arr(2)%e /= 4)) error stop 22
        if (y%arr(2)%r3 /= 7) error stop 23
        y%arr(2)%e(1,1) = 0
        if (x%arr(2)%e(1,1) /= 4) error stop 24
        print *, "module2 work ok"
    end subroutine

    subroutine block_work()
        block
            ! Same type names as in derived_types_250_module1, different layout
            type :: node
                real(8) :: r1 = 0, r2 = 0, r3 = 0
                character(len=:), allocatable :: s
                real(8), allocatable :: e(:,:)
            end type
            type :: holder
                type(node), allocatable :: arr(:)
            end type
            type(holder) :: x, y
            allocate(x%arr(2))
            x%arr(2)%s = "hello"
            allocate(x%arr(2)%e(2,2))
            x%arr(2)%e = 4
            x%arr(2)%r3 = 7
            y = x
            if (size(y%arr) /= 2) error stop 250
            if (y%arr(2)%s /= "hello") error stop 251
            if (any(y%arr(2)%e /= 4)) error stop 252
            if (y%arr(2)%r3 /= 7) error stop 253
            y%arr(2)%e(1,1) = 0
            if (x%arr(2)%e(1,1) /= 4) error stop 254
            print *, "module2 block ok"
        end block
    end subroutine
end module
