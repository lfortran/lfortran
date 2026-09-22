! A DEFERRED PROCEDURE statement (R1622 of the Fortran 2028 working draft,
! J3/26-007r1, 16.4.1.4) declares deferred procedures:
!
!     deferred-proc-decl-stmt is DEFERRED PROCEDURE ( interface-name )
!                                [ :: ] deferred-proc-name-list
!
! C1622 requires a deferred-proc-name to be the name of a deferred procedure,
! that is a deferred argument of the scoping unit, so the statement only has a
! meaning in a requirement (R1634), a template (R1605) or a templated procedure
! (R1613). The interface-name must name an interface, and every spelling that
! does not is diagnosed rather than crashing the compiler.
!
! See integration_tests/template_deferred_proc_01.f90 for the accepted
! spellings.

module deferred_proc_decl_1_mod
    implicit none

    abstract interface
        subroutine iface(x)
            integer, intent(in) :: x
        end subroutine
    end interface

    integer :: not_an_interface

    ! C1622: 'q' is not among the deferred arguments of the requirement.
    requirement r1 {p}
        deferred procedure (iface) :: p, q  ! {Error} 'q' is not a deferred argument of 'r1'
    end requirement

    ! The interface-name does not resolve to anything.
    requirement r2 {p}
        deferred procedure (no_such_iface) :: p  ! {Error} the interface 'no_such_iface' is not declared
    end requirement

    ! The interface-name resolves, but not to an interface.
    template t1(p)
        deferred procedure (not_an_interface) :: p  ! {Error} 'not_an_interface' is not an interface
    end template

    ! A module has no deferred arguments.
    deferred procedure (iface) :: p  ! {Error} a deferred procedure can only be declared in a requirement, a template or a templated procedure

end module

program deferred_proc_decl_1
    implicit none

    ! Neither has a main program.
    deferred procedure (iface) q  ! {Error} a deferred procedure can only be declared in a requirement, a template or a templated procedure

end program
