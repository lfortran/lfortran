! The bound of a local array calls functions of its module, so a pass adds a
! helper function to the module in every translation unit that compiles it,
! including one that only loads it from the modfile.
module separate_compilation_60a
implicit none
type :: t
    integer :: v
end type
contains
    pure function construct(k) result(r)
        integer, intent(in) :: k
        type(t) :: r
        r%v = k
    end function

    pure integer function comp(x)
        type(t), intent(in) :: x
        comp = x%v
    end function

    subroutine s(x)
        real, intent(inout) :: x(:)
        real :: tmp(comp(construct(3)))
        tmp = 2.0
        x = sum(tmp)
    end subroutine
end module
