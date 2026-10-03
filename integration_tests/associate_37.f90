! Test: type-bound procedure call on associate variable whose selector is
! a function call that takes a procedure-pointer argument from the parent
! scope. Regression test for a compile-time segfault (null `tmp` in
! convert_call_args) caused by NestedVarVisitor not traversing
! AssociateBlockCall bodies.
module associate_37_mod
    implicit none

    abstract interface
        pure function init_i(x) result(f)
            double precision, intent(in) :: x(:)
            double precision, allocatable :: f(:)
        end function
    end interface

    type :: t
        double precision, allocatable :: v(:)
    contains
        procedure :: get_v
    end type

contains

    pure function get_v(self) result(r)
        class(t), intent(in) :: self
        double precision, allocatable :: r(:)
        r = self%v
    end function

    function make_t(init) result(res)
        procedure(init_i), pointer, intent(in) :: init
        type(t) :: res
        res%v = init([1d0, 2d0, 3d0])
    end function

    pure function id(x) result(f)
        double precision, intent(in) :: x(:)
        double precision, allocatable :: f(:)
        f = x
    end function

end module

program associate_37
    use associate_37_mod
    implicit none
    procedure(init_i), pointer :: f
    double precision, allocatable :: r(:)
    f => id
    call run()
contains
    subroutine run()
        associate(s => make_t(f))
            r = s%get_v()
        end associate
        print *, r
        if (size(r) /= 3) error stop
        if (abs(r(1) - 1d0) > 1d-12) error stop
        if (abs(r(2) - 2d0) > 1d-12) error stop
        if (abs(r(3) - 3d0) > 1d-12) error stop
    end subroutine
end program
