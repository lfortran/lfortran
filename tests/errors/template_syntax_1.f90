! A TEMPLATE statement spells its deferred argument list with braces in the
! Fortran 2028 working draft (J3/26-007r1, 16.1.1), as corrected by J3 paper
! 26-158 ("Syntax correction for template deferred args"):
!
!     R1602 template-stmt  is  TEMPLATE template-name { [ deferred-arg-name-list ] }
!
! LFortran used to spell it with parentheses; this test pins that the
! parenthesised list is now a syntax error, with and without arguments. See
! integration_tests/template_simple_01.f90 for the accepted spelling.
!
! REQUIREMENT (R1633), REQUIRE (R1636), INSTANTIATE (R1625) and a templated
! subprogram (R1611, R1612) already use braces, so a deferred or instantiation
! argument list is now always `{...}` and an ordinary dummy argument list is
! always `(...)`.

module template_syntax_1

    requirement r {t}
        deferred type :: t
    end requirement

    template one_arg_tmpl(u)  ! {Error} Token '(' is unexpected here
        deferred type :: u
        require :: r {u}
    end template

    template no_arg_tmpl()  ! {Error} Token '(' is unexpected here
        integer, parameter :: n = 1
    end template

end module
