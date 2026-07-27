program arrays_reshape_45
    implicit none

    type :: item
        integer :: value
    end type

    type(item) :: source(2, 2)

    source(1, 1)%value = 1
    source(2, 1)%value = 2
    source(1, 2)%value = 3
    source(2, 2)%value = 4

    source = reshape(transpose(source), [2, 2])

    if (source(1, 1)%value /= 1) error stop
    if (source(2, 1)%value /= 3) error stop
    if (source(1, 2)%value /= 2) error stop
    if (source(2, 2)%value /= 4) error stop
    print *, "test passed"
end program arrays_reshape_45
