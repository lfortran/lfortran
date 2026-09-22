! R1634 of the Fortran 2028 working draft (J3/26-007r1, 16.6) lists exactly what
! a REQUIREMENT construct may contain:
!
!     R1634 requirement-specification  is  deferred-arg-decl-stmt
!                                      or  interface-block
!
! A bare subprogram body is neither, so the deferred procedures of a requirement
! have to be declared by an interface block. LFortran used to accept the bare
! spelling; this test pins that it is now a syntax error, for a function and for
! a subroutine alike. See integration_tests/template_deferred_interface_01.f90
! for the accepted spelling, a DEFERRED INTERFACE block.
!
! A subprogram statement opens a construct, so each syntax error below also
! leaves the matching END statement unexpected.

module requirement_bare_subprogram_1

    requirement func_r {t, f}
        deferred type :: t
        function f(x) result(z)  ! {Error} Token 'f' (of type 'identifier') is unexpected here
            type(t), intent(in) :: x
            type(t) :: z
        end function  ! {Error} Token 'end function' is unexpected here
    end requirement

    requirement sub_r {t, s}
        deferred type :: t
        subroutine s(x)  ! {Error} Token 's' (of type 'identifier') is unexpected here
            type(t), intent(in) :: x
        end subroutine  ! {Error} Token 'end subroutine' is unexpected here
    end requirement

end module
