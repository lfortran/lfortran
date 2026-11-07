! Specific procedures of a generic distinguishable only by the kind type
! parameter of a parameterized derived type: of the passed-object dummy
! argument of a type-bound generic, or of a type inherited by an extension.
module generic_interface_distinguishable_03_m
    implicit none
    integer :: which = 0
    type :: pt(k)
        integer, kind :: k
        real(k) :: v
    contains
        procedure :: a4
        procedure :: a8
        generic :: g => a4, a8
    end type
    type, extends(pt) :: ch
    end type

    interface h
        module procedure s4, s8
    end interface
contains
    subroutine a4(self, n)
        class(pt(4)), intent(in) :: self
        integer, intent(in) :: n
        which = 4 + 0 * n
    end subroutine
    subroutine a8(self, n)
        class(pt(8)), intent(in) :: self
        integer, intent(in) :: n
        which = 8 + 0 * n
    end subroutine
    subroutine s4(x)
        type(ch(4)), intent(in) :: x
        which = 14
    end subroutine
    subroutine s8(x)
        type(ch(8)), intent(in) :: x
        which = 18
    end subroutine
end module

program generic_interface_distinguishable_03
    use generic_interface_distinguishable_03_m
    implicit none
    type(pt(4)) :: x
    type(pt(8)) :: y
    call x%g(1); if (which /= 4) error stop
    call y%g(1); if (which /= 8) error stop
    print *, "ok"
end program
