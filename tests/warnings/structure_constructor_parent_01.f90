program structure_constructor_parent_01
    implicit none
    type :: base_t
        integer :: x = 1
    end type
    type, extends(base_t) :: e_t
        integer :: z = 5
    end type
    type, extends(e_t) :: e2_t
        integer :: w = 7
    end type
    type(e_t) :: e
    type(e2_t) :: e2
    e = e_t(base_t(11), 51)
    print *, e%x, e%z
    e2 = e2_t(base_t(21), 52, 72)
    print *, e2%x, e2%z, e2%w
end program
