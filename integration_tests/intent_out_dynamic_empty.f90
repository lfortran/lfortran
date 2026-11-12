program intent_out_dynamic_empty
    use intent_out_dynamic_empty_base
    implicit none
    type :: payload
        integer :: n = 7
        integer, allocatable :: values(:)
    end type
    type, extends(empty) :: extended
        type(payload) :: part
    end type
    type, extends(extended) :: leaf
    end type
    type(empty) :: a
    type(leaf) :: b
    call reset(a)
    b%part%n = 19
    allocate(b%part%values(3))
    call reset(b)
    if (b%part%n /= 7) error stop 1
    if (allocated(b%part%values)) error stop 2
end program
