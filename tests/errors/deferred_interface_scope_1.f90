! Each interface body of a DEFERRED INTERFACE block (R1503 of the Fortran 2028
! working draft, J3/26-007r1) declares a deferred procedure, which is a
! deferred argument (16.4.1.4):
!
!     A deferred procedure is a deferred argument that is declared to be a
!     procedure. Its interface is defined by a deferred-proc-decl-stmt or an
!     interface-body in a deferred interface block.
!
! Only a requirement (R1634), a template (R1605) and a templated procedure
! (R1613) have deferred arguments, so a deferred interface block has no
! meaning in any other scoping unit. This mirrors C1613, which constrains a
! DEFERRED TYPE statement the same way.
!
! See integration_tests/template_deferred_interface_01.f90 for the accepted
! spellings in a requirement and in a template.

module deferred_interface_scope_1_mod
    implicit none

    ! A module has no deferred arguments.
    deferred interface  ! {Error} a deferred interface can only appear in a requirement, a template or a templated procedure
        function f(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function
    end interface

end module

module deferred_interface_scope_1_items_mod
    implicit none

    ! A deferred procedure is declared by an interface body; a procedure
    ! statement names procedures that are declared elsewhere, so it cannot
    ! appear in a deferred interface block.
    requirement r {t, f}
        deferred type :: t
        deferred interface
            procedure f  ! {Error} a deferred interface block can only contain interface bodies
        end interface
    end requirement

end module

program deferred_interface_scope_1
    implicit none

    ! Neither has a main program.
    deferred interface  ! {Error} a deferred interface can only appear in a requirement, a template or a templated procedure
        subroutine s(x)
            integer, intent(in) :: x
        end subroutine
    end interface

end program
