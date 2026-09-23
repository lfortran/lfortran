! The deferred argument list of a REQUIREMENT construct and the instantiation
! argument list of a REQUIRE statement are written with curly braces in the
! Fortran 2028 working draft (J3/26-007r1, 16.6 and 16.7):
!
!     R1633 requirement-stmt  is  REQUIREMENT requirement-name
!               { [ deferred-arg-name-list ] }
!     R1636 require-stmt      is  REQUIRE [ :: ] requirement-name
!               { [ instantiation-arg-spec-list ] }
!
! LFortran used to spell both lists with parentheses; this test pins that the
! parenthesised spelling is now a syntax error. See
! integration_tests/template_07.f90 for the accepted spellings.

module requirement_syntax_1

    requirement r {t}
        deferred type :: t
    end requirement

    template tmpl {u}
        require :: r(u)  ! {Error} Token '(' is unexpected here
    end template

    ! Placed last: the requirement statement opens a construct, so its syntax
    ! error also leaves the matching END REQUIREMENT unexpected.
    requirement r_paren(t)  ! {Error} Token '(' is unexpected here
        deferred type :: t
    end requirement  ! {Error} Token 'requirement' is unexpected here

end module
