program allocate_74
    implicit none
    type :: v
        character(len=:), allocatable :: d(:)
    end type
    type(v) :: b
    type(v) :: c(2)

    allocate(character(len=2) :: b%d(3))
    b%d = ['ab', 'cd', 'ef']
    if (any(b%d /= ['ab', 'cd', 'ef'])) error stop 1
end program
