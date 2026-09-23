! A REQUIRE statement names exactly one requirement in the Fortran 2028
! working draft (J3/26-007r1, 16.7):
!
!     R1636 require-stmt  is  REQUIRE [ :: ] requirement-name
!               { [ instantiation-arg-spec-list ] }
!
! and 16.7 NOTE 2 writes two requirements as two separate REQUIRE statements.
! LFortran used to accept a comma separated list of requirements in one
! statement; this test pins that the list is now a syntax error, with and
! without the optional `::`. See integration_tests/template_07.f90 for the
! accepted spellings.

module require_syntax_1

    requirement r1 {t}
        deferred type :: t
    end requirement

    requirement r2 {u}
        deferred type :: u
    end requirement

    template tmpl_1 {v}
        deferred type :: v
        require :: r1 {v}, r2 {v}  ! {Error} Token ',' is unexpected here
    end template

    template tmpl_2 {v}
        deferred type :: v
        require r1 {v}, r2 {v}  ! {Error} Token ',' is unexpected here
    end template

end module
