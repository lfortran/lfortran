! C1613 of the Fortran 2028 working draft (J3/26-007r1, 16.4.1.2):
!
!     A deferred-arg-name in a deferred-type-declaration-stmt shall be the name
!     of a deferred argument of the scoping unit containing the statement.
!
! Only a requirement (R1634), a template (R1605) and a templated procedure
! (R1613) have a deferred-argument list, so a DEFERRED TYPE statement is
! invalid in any other scoping unit. LFortran used to accept it there and
! silently create an ordinary, empty derived type instead.
!
! See integration_tests/template_deferred_type_01.f90 for the accepted
! spellings in a requirement and in a template, and
! integration_tests/template_instantiate_subp_01.f90 for a templated
! procedure.

module deferred_type_scope_1_mod
    implicit none

    ! A module has no deferred arguments.
    deferred type :: t  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure

contains

    ! Neither has an ordinary subprogram.
    subroutine s()
        deferred type :: u  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure
    end subroutine

end module

module deferred_type_scope_1_args_mod
    implicit none

    ! The declared name has to be one of the deferred arguments of the
    ! enclosing requirement, template or templated procedure.
    requirement r {t}
        deferred type :: t
        deferred type :: y  ! {Error} 'y' is not a deferred argument of 'r'
    end requirement

    template tmpl(t)
        deferred type :: t
        deferred type :: z  ! {Error} 'z' is not a deferred argument of 'tmpl'
    end template

contains

    template subroutine swap{t}(x, y)
        deferred type :: t
        deferred type :: w  ! {Error} 'w' is not a deferred argument of 'swap'
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

end module

program deferred_type_scope_1
    implicit none

    ! A main program has no deferred arguments either.
    deferred type :: v  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure

end program
