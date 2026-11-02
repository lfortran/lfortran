module derived_types_251_a_b
    implicit none
    type :: node
        real(8) :: r1 = 0, r2 = 0, r3 = 0
        character(len=:), allocatable :: s
        real(8), allocatable :: e(:,:)
    end type
    type :: holder2
        type(node), allocatable :: arr(:)
    end type
contains
    subroutine work2()
        type(holder2) :: x, y
        allocate(x%arr(2))
        x%arr(2)%s = "hello"
        allocate(x%arr(2)%e(2,2))
        x%arr(2)%e = 4
        x%arr(2)%r3 = 7
        y = x
        if (y%arr(2)%s /= "hello") error stop 21
        if (any(y%arr(2)%e /= 4)) error stop 22
        if (y%arr(2)%r3 /= 7) error stop 23
        y%arr(2)%e(1,1) = 0
        if (x%arr(2)%e(1,1) /= 4) error stop 24
        print *, "derived_types_251_a_b ok"
    end subroutine
end module
