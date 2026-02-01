program class_101
    implicit none
    type, abstract :: foo
    end type foo
    type, extends(foo) :: bar
    end type bar
    type(bar), allocatable :: x
    class(foo), allocatable :: y
    call move_alloc(x, y)
    if (allocated(y)) error stop "Y SHOULD BE UNALLOCATED"
    print "(A)", "PASSED: move_alloc with unallocated polymorphic type"
end program class_101
