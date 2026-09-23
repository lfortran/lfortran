! A deferred type argument of a template or a requirement may only be used in a
! CLASS declaration if it is extensible (Fortran 2028 working draft J3/26-007r1,
! 16.4.1.2):
!
!     A deferred type with the EXTENSIBLE attribute is an extensible type. A
!     deferred type with the ABSTRACT attribute is an abstract type. A deferred
!     type with the ABSTRACT attribute implicitly has the EXTENSIBLE attribute.
!
! NOTE 4 of that subclause lists the valid combinations: for a bare
! `DEFERRED TYPE :: t1`, `TYPE(t1)` is valid and `CLASS(t1)` is not.
!
! CommonVisitor::determine_type resolved the CLASS type name and cast the
! resulting symbol to a derived type without checking, so a deferred type
! reached an internal assertion instead of a diagnostic; in a Release build,
! where the assertion is compiled out, the bad cast was used instead.
!
! The EXTENSIBLE and ABSTRACT attributes are not implemented yet
! (lfortran/lfortran#13285), so no deferred type is extensible today and every
! CLASS declaration of one is rejected. See
! integration_tests/template_deferred_type_01.f90 for the accepted TYPE(t)
! spelling, which stays valid.

module deferred_type_class_1
    implicit none

    requirement r {t, s1}
        deferred type :: t
        interface
            subroutine s1(x)
                class(t), intent(in) :: x  ! {Error} deferred type 't' is not extensible, so it cannot be used in a class declaration
            end subroutine
        end interface
    end requirement

    template tmpl(u)
        deferred type :: u
    contains
        subroutine s2(y)
            class(u), intent(in) :: y  ! {Error} deferred type 'u' is not extensible, so it cannot be used in a class declaration
        end subroutine
    end template

end module
