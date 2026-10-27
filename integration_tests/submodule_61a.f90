! Used only by the submodule, not by the program or the parent module. Its
! variables need setting up at run time.
module submodule_61_r
    implicit none
    type :: tc
        character(len=4) :: nm = "abcd"
        integer :: k = 3
    end type
    type :: tp
        integer, pointer :: q(:) => null()
    end type
    integer :: r_count = 0
    character(len=6) :: r_tag = "rrrrrr"
    type(tc) :: r_objs(2) = tc("wxyz", 5)
    type(tp) :: r_obj
contains
    integer function r_next()
        integer, save :: k = 10
        k = k + 1
        r_count = r_count + 1
        r_next = k
    end function
end module
