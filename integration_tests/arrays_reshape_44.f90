program arrays_reshape_44

    implicit none

    type :: node
        integer :: value
    end type

    type(node), parameter :: items(4) = node(7)
    type(node), parameter :: grid(2,2) = reshape(items, [2,2])

    if(grid(2,2)%value /= 7) error stop
    print *,"test passed"
end program arrays_reshape_44
