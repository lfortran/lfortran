program structure_constructor_parent_01
    implicit none
    type :: base_t
        integer :: x = 1
    end type
    type, extends(base_t) :: e_t
        integer :: z = 5
    end type
    type(e_t) :: e
    e = e_t(base_t(11), 51)
    print *, e%x, e%z
end program
