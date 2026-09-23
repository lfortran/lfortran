! A deferred type argument of a template or a requirement is declared with the
! DEFERRED TYPE statement of the Fortran 2028 working draft (J3/26-007, 16.4.1.2):
!
!     R1616 deferred-type-declaration-stmt  is  DEFERRED TYPE
!               [, deferred-type-attr-list ] :: deferred-arg-name-list
!
! DEFERRED is not a type-attr-spec (R739 lists ABSTRACT, access-spec, BIND(C),
! EXTENDS, PURE and SIMPLE only), so a statement beginning `TYPE ,` can only
! open a derived-type definition and `type, deferred :: t` is not valid in any
! context. LFortran used to accept that spelling; this test pins that it is now
! a syntax error. See integration_tests/template_deferred_type_01.f90 for the
! accepted spelling.

module deferred_type_syntax_1

    requirement r {t}
        type, deferred :: t  ! {Error} Token 'deferred' is unexpected here
    end requirement

    template tmpl {u}
        type, deferred :: u  ! {Error} Token 'deferred' is unexpected here
    end template

    ! Outside a template or a requirement it was never meaningful either.
    type, deferred :: v  ! {Error} Token 'deferred' is unexpected here

end module
