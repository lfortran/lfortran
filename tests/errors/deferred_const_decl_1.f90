! The constraints on a deferred constant declaration (Fortran 2028 working
! draft J3/26-007r1, 16.4.1.3):
!
!     R1618 deferred-const-decl-stmt  is  DEFERRED declaration-type-spec,
!               deferred-const-attr-spec-list :: deferred-const-entity-decl-list
!     R1619 deferred-const-attr-spec   is  DIMENSION ( array-spec )
!                                      or  PARAMETER
!                                      or  rank-clause
!     R1620 deferred-const-entity-decl is  deferred-const-name [ ( array-spec ) ]
!
!     C1618 A deferred-const-attr-spec-list shall specify the PARAMETER attribute.
!     C1619 The declaration-type-spec in a deferred-const-decl-stmt shall specify
!           type integer, logical, or character. If it specifies type character,
!           it shall specify that the character length is assumed.
!     C1620 A deferred-const-name shall be the name of a deferred constant.
!     C1621 An array-spec in a deferred-const-decl-stmt shall be an
!           implied-shape-spec, assumed-implied-spec, explicit-shape-spec-list,
!           or explicit-shape-bounds-spec. It shall not explicitly specify any
!           lower bound.
!
! See integration_tests/template_deferred_const_01.f90 for the accepted forms.

module deferred_const_decl_1
    implicit none

    ! A deferred constant is a deferred argument, so the statement is only
    ! meaningful where deferred arguments are declared (R1615). This case comes
    ! first because a requirement whose specification fails leaves the "inside a
    ! requirement" state set.
    deferred integer, parameter :: n  ! {Error} a `deferred` declaration is only allowed in a requirement, a template or a templated subprogram

    ! C1618: the attribute list must specify PARAMETER.
    requirement c1618 {a}
        deferred integer :: a  ! {Error} a `deferred` constant declaration must specify the `parameter` attribute
    end requirement

    ! C1619: only integer, logical and character are allowed.
    requirement c1619_real {b}
        deferred real, parameter :: b  ! {Error} the type of a `deferred` constant must be integer, logical or character
    end requirement

    requirement c1619_type {c}
        deferred type(t), parameter :: c  ! {Error} the type of a `deferred` constant must be integer, logical or character
    end requirement

    ! C1619: a character deferred constant must have assumed length.
    requirement c1619_char {d}
        deferred character(5), parameter :: d  ! {Error} a `deferred` character constant must have assumed length, declared as `character(*)`
    end requirement

    ! C1620: the name must be a deferred argument of the containing scoping unit.
    requirement c1620 {e}
        deferred integer, parameter :: f  ! {Error} 'f' is not a deferred argument of this template or requirement
    end requirement

    ! C1621: an explicit lower bound is not allowed, in any of its spellings.
    requirement c1621_lbound {g}
        deferred integer, parameter :: g(1:3)  ! {Error} a `deferred` constant array must not specify a lower bound; its lower bounds are always one
    end requirement

    requirement c1621_lbound_star {h}
        deferred integer, parameter :: h(2:*)  ! {Error} a `deferred` constant array must not specify a lower bound; its lower bounds are always one
    end requirement

    ! C1621: an assumed- or deferred-shape spec is not one of the four allowed
    ! array-spec forms; a named constant cannot have one.
    requirement c1621_assumed {i}
        deferred integer, parameter :: i(:)  ! {Error} a `deferred` constant must not have an assumed or deferred shape `:`
    end requirement

    ! C1621: an assumed-size spec is not one of the four allowed forms either.
    requirement c1621_assumed_size {j}
        deferred integer, parameter :: j(3,*)  ! {Error} the dimensions of a `deferred` constant must be either all upper bounds, as in `(3,4)`, or all `*`, as in `(*,*)`
    end requirement

    ! An implied-rank-spec is the whole array-spec (F2028 C835).
    requirement c1621_rank {k}
        deferred integer, parameter :: k(.., ..)  ! {Error} `..` must be the only dimension of a `deferred` constant
    end requirement

    ! R1620 has no initializer: the value comes from the instantiation argument.
    requirement init {m}
        deferred integer, parameter :: m = 3  ! {Error} a `deferred` constant must not be given a value; its value comes from the instantiation argument
    end requirement

    ! The array forms of NOTE 2 of 16.4.1.3 all parse and satisfy C1621, but an
    ! array deferred constant is not implemented yet. This pins which spellings
    ! reach the semantic stage; the accepted-and-working forms are the scalars in
    ! integration_tests/template_deferred_const_01.f90.
    template note2(x2, x3, x4, x5, x6, x7)
        integer, parameter :: v1(2) = [5,15]   ! not a deferred constant
        deferred integer, parameter :: x2(3)  ! {Error} a `deferred` constant that is an array is not supported yet
        deferred integer, parameter :: x3(v1)  ! {Error} a `deferred` constant that is an array is not supported yet
        deferred integer, parameter :: x4(*)  ! {Error} a `deferred` constant that is an array is not supported yet
        deferred integer, parameter :: x5(*,*)  ! {Error} a `deferred` constant that is an array is not supported yet
        deferred integer, parameter, rank(2) :: x6  ! {Error} a `deferred` constant that is an array is not supported yet
        deferred integer, parameter :: x7(..)  ! {Error} a `deferred` constant that is an array is not supported yet
    end template

    ! The DIMENSION attribute spelling of the same array-spec (R1619).
    requirement dim_attr {p}
        deferred integer, parameter, dimension(3) :: p  ! {Error} a `deferred` constant that is an array is not supported yet
    end requirement

    ! R1619's DIMENSION attribute and R1620's per-entity array-spec at once.
    requirement dim_twice {q}
        deferred integer, parameter, dimension(3) :: q(2)  ! {Error} the rank of 'q' is specified twice
    end requirement

end module
