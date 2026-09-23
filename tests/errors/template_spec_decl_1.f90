! C1603 and C1604 of the Fortran 2028 working draft (J3/26-007r1, 16.1.1):
!
!     C1603  If a template-specification is a type declaration statement, it
!            shall specify the PARAMETER attribute.
!     C1604  If a template-specification is a procedure declaration statement,
!            it shall not specify the POINTER attribute.
!
! with the accompanying note: a template specification part cannot declare a
! variable or procedure pointer.
!
! Both constraints apply to the template-specifications only, that is to the
! items between the `template` statement and `contains`. The contained
! procedures are ordinary subprogram bodies and may declare locals, and a
! deferred argument declaration (R1615) or a requirement is not a
! template-specification either.

module template_spec_decl_1_mod
    implicit none

    abstract interface
        subroutine iface_sub()
        end subroutine
    end interface

    requirement plus_r {t, plus_t}
        deferred type :: t
        deferred interface
            function plus_t(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
    end requirement

    template tmpl(t, plus_t, n)
        ! Not template-specifications, and so not restricted by C1603: a
        ! deferred type declaration, a deferred constant and a requirement.
        deferred type :: t
        deferred integer, parameter :: n
        require :: plus_r {t, plus_t}

        ! A named constant is what C1603 permits.
        integer, parameter :: repeat_count = 2

        ! A procedure declaration without POINTER is what C1604 permits.
        procedure(iface_sub) :: plain_proc

        private
        public :: add_n_times

        integer :: bad_variable          ! {Error} a template specification part cannot declare a variable, so 'bad_variable' must have the parameter attribute
        procedure(iface_sub), pointer :: bad_proc_ptr          ! {Error} a template specification part cannot declare a procedure pointer, so 'bad_proc_ptr' must not have the pointer attribute
    contains
        function add_n_times(x) result(z)
            type(t), intent(in) :: x
            type(t) :: z
            ! A local variable in a contained procedure stays legal.
            integer :: i
            z = x
            do i = 1, n * repeat_count
                z = plus_t(z, x)
            end do
        end function
    end template

end module
