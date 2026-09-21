! The instantiation argument list of an INSTANTIATE statement is written with
! curly braces in the Fortran 2028 working draft (J3/26-007r1, 16.5.1):
!
!     R1625 template-instantiate-stmt  is  INSTANTIATE [ :: ] template-construct-name
!               { [ instantiation-arg-spec-list ] } [ , rename-list ]
!           or                               INSTANTIATE [ :: ] template-construct-name
!               { [ instantiation-arg-spec-list ] }, ONLY : [ only-list ]
!
! LFortran used to spell that list with parentheses; this test pins that the
! parenthesised spelling is now a syntax error. See
! integration_tests/template_07.f90 for the accepted spellings.

module instantiate_syntax_1

    requirement r {t}
        deferred type :: t
    end requirement

    template tmpl(t)
        require r {t}
        private
        public :: id
    contains
        function id(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = x
        end function
    end template

contains

    subroutine test()
        instantiate tmpl(integer)  ! {Error} Token '(' is unexpected here
        instantiate tmpl(real), only: id_real => id  ! {Error} Token '(' is unexpected here
    end subroutine

end module
