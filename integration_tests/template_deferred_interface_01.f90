! Tests the DEFERRED INTERFACE form of an interface block (Fortran 2028 draft
! J3/26-007r1, R1503:
!     interface-stmt is INTERFACE [ generic-spec ]
!                    or ABSTRACT INTERFACE
!                    or DEFERRED INTERFACE
! ). Every interface body of such a block declares a deferred procedure
! (16.4.1.4), and C1637 requires the interface block of a
! requirement-specification to be an ABSTRACT or a DEFERRED one. Both a
! requirement and a template can declare a deferred procedure this way.
module template_deferred_interface_01_m
    implicit none
    private
    public :: test_deferred_interface

    requirement add_r {t, add}
        deferred type :: t
        deferred interface
            pure function add(lhs, rhs) result(res)
                type(t), intent(in) :: lhs
                type(t), intent(in) :: rhs
                type(t) :: res
            end function
        end interface
    end requirement

    ! The deferred procedure of the requirement comes in through REQUIRE, and
    ! the template declares a second deferred procedure of its own.
    template twice_tmpl {t, add, show}
        deferred type :: t
        require :: add_r {t, add}
        deferred interface
            subroutine show(arg)
                type(t), intent(in) :: arg
            end subroutine
        end interface
        private
        public :: twice
      contains
        function twice(arg) result(res)
            type(t), intent(in) :: arg
            type(t) :: res
            res = add(arg, arg)
            call show(res)
        end function
    end template

contains

    subroutine show_integer(arg)
        integer, intent(in) :: arg
        print *, "integer:", arg
    end subroutine

    subroutine show_real(arg)
        real, intent(in) :: arg
        print *, "real:", arg
    end subroutine

    subroutine test_deferred_interface()
        instantiate twice_tmpl {integer, operator(+), show_integer}, &
            only: twice_integer => twice
        instantiate twice_tmpl {real, operator(+), show_real}, &
            only: twice_real => twice
        integer :: i
        real :: r
        i = twice_integer(21)
        if (i /= 42) error stop
        r = twice_real(1.25)
        if (abs(r - 2.5) > 1e-6) error stop
    end subroutine

end module

program template_deferred_interface_01
    use template_deferred_interface_01_m
    implicit none

    call test_deferred_interface()

end program
