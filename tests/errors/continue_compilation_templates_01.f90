module continue_compilation_templates_01_mod
    implicit none

    ! Duplicate parameter name in requirement's namelist
    requirement dup_param_req {T, T, op_func}
        deferred type :: T
        deferred interface
            function op_func(x) result(y)
                type(T), intent(in) :: x
                type(T) :: y
            end function
        end interface
    end requirement

    ! A second, independent duplicate-parameter requirement, to verify
    ! compilation continues past the first error above and still
    ! reports this one too.
    requirement dup_param_req2 {V, V, W, comp_func}
        deferred type :: V
        deferred type :: W
        deferred interface
            function comp_func(x, y) result(z)
                type(V), intent(in) :: x
                type(V), intent(in) :: y
                logical :: z
            end function
        end interface
    end requirement

    ! A recoverable error inside a template body. The symbol table visitor adds
    ! the Template symbol only after the whole template is built, so letting the
    ! abort escape left the module without it and the body visitor then looked
    ! it up and asserted. Two errors, to show the template keeps being processed
    ! past the first one.
    ! C1603 restricts a template specification part to declarations with the
    ! PARAMETER attribute, so the two erroneous declarations below are named
    ! constants.
    template redecl_tmpl {T}
        deferred type :: T
        integer, parameter :: n = 1
        real, parameter :: n = 1.0
        integer, parameter :: bad = "abc"
    end template

end module continue_compilation_templates_01_mod

! template_error_01a
module math

    implicit none
    private
    public :: add_real, slash_real

contains

    pure function add_real(x, y) result(total)
        real, intent(in) :: x, y
        real :: total
        total = x + y
    end function

    pure function slash_real(x, y) result(total)
        real, intent(in) :: x, y
        real :: total
        total = x / y
    end function

end module

module travel

    use math
    implicit none
    private 
    public :: travel_tmpl

    requirement operations {D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S}
        deferred type :: D
        deferred type :: T
        deferred type :: S

        deferred interface
            pure function plus_D(l, r) result(total)
                type(D), intent(in) :: l, R
                type(D) :: total
            end function

            pure function plus_T(l, r) result(total)
                type(T), intent(in) :: l, R
                type(T) :: total
            end function

            pure function D_divided_by_T(n, d) result(quotient)
                type(D), intent(in) :: n
                type(T), intent(in) :: d
                type(S) :: quotient
            end function

            pure function D_divided_by_S(n, d) result(quotient)
                type(D), intent(in) :: n
                type(S), intent(in) :: d
                type(T) :: quotient
            end function
        end interface
    end requirement

    template travel_tmpl {D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S}
        require :: operations {D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S}
        private
        public :: avg_S_from_T
    contains
        pure function avg_S_from_T(d1, t1, d2, t2) result(avg)
            type(D), intent(in) :: d1, d2
            type(T), intent(in) :: t1, t2
            type(S) :: avg
            avg = D_divided_by_T(plus_D(d1, d2), plus_T(t1, t2))
        end function
        
        pure function avg_S_from_S(d1, s1, d2, s2) result(avg)
            type(D), intent(in) :: d1, d2
            type(S), intent(in) :: s1, s2
            type(S) :: avg
            avg = avg_S_from_T(d1, D_divided_by_T(d1, s1), d2, D_divided_by_S(d2, s2))
        end function
    end template

end module


! template_error_01b
module template_travel_01b_m

    implicit none
    private
    public :: travel_tmpl

    requirement operation {A, B, C, op}
        deferred type :: A
        deferred type :: B
        deferred type :: C

        deferred interface
            pure function op(l, r) result(res)
                type(A), intent(in) :: l
                type(B), intent(in) :: r
                type(C) :: res
            end function
        end interface
    end requirement

    template travel_tmpl {D, T, S, plus_D, plus_T, D_divided_by_T, D_divided_by_S}
        require :: operation {D, D, D, plus_D}
        !require :: operation {T, T, T, plus_T}
        require :: operation {D, T, S, D_divided_by_T}
        require :: operation {D, S, T, D_divided_by_S}
        private
        public :: avg_S_from_T, avg_S_from_S
    contains
        pure function avg_S_from_T(d1, t1, d2, t2) result(avg)
            type(D), intent(in) :: d1, d2
            type(T), intent(in) :: t1, t2
            type(S) :: avg
            avg = D_divided_by_T(plus_D(d1, d2), plus_T(t1, t2))
        end function

        pure function avg_S_from_S(d1, s1, d2, s2) result(avg)
            type(D), intent(in) :: d1, d2
            type(S), intent(in) :: s1, s2
            type(S) :: avg
            avg = avg_S_from_T(d1, D_divided_by_S(d1, s1), d2, D_divided_by_S(d2, s2))
        end function
    end template

end module

! template_error_02
module template_error_02_m
    implicit none
    private
    public :: add_t

    requirement R {T, F} 
        deferred type :: T
        deferred interface
            function F(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, F}
        require :: R {T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains
    
    real function func_arg_real(x, y, a) result(z)
        real, intent(in) :: x, y, a
        z = x + y
    end function

    subroutine test_template()
        instantiate add_t {real, func_arg_real}, only: add_real => add_generic
    end subroutine
end module

! template_error_03
module template_error_03_m
    implicit none
    private
    public :: add_t

    requirement R {T, F} 
        deferred type :: T
        deferred interface
            function F(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, F}
        require :: R {T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains
    
    subroutine func_arg_real(x, y)
        real, intent(in) :: x, y
    end subroutine

    subroutine test_template()
        instantiate add_t {real, func_arg_real}, only: add_real => add_generic
    end subroutine
end module

! template_error_04
module template_error_04_m
    implicit none
    private
    public :: add_t

    requirement R {T, F} 
        deferred type :: T
        deferred interface
            function F(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, F}
        require :: R {T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains
    
    real function func_arg_real(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine test_template()
        instantiate add_t {real, func_arg_real}, only: add_real => add_generic
    end subroutine
end module

! template_error_05
module template_error_05_m
    implicit none
    private
    public :: add_t

    requirement R {T, F} 
        deferred type :: T
        deferred interface
            function F(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, F}
        require :: R {T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains
    
    integer function func_arg_real(x, y) result(z)
        real, intent(in) :: x, y
        z = x + y
    end function

    subroutine test_template()
        instantiate add_t {real, func_arg_real}, only: add_real => add_generic
    end subroutine
end module

! template_error_06
module template_error_06_m
  implicit none
  private
  public :: struct_t

  requirement r {t}
      deferred type :: t
  end requirement

  template struct_t {t}
      require :: r {t}
      private
      public :: tuple

      type :: tuple
          type(g) :: fst
          type(g) :: snd
      end type

  contains

  end template

contains

end module

! template_error_07a
module template_error_07a_m
    implicit none
    private
    public :: op_t

    requirement semigroup {t}
        deferred type :: t
        deferred interface
            elemental function combine(x, y) result(combined)
                type(t), intent(in) :: x, y
                type(t) :: combined
            end function
        end interface
    end requirement

  contains
    
    subroutine test_template()
    end subroutine
    
end module

! template_error_07b
module template_error_07b_m
    implicit none
    private
    public :: op_t

    requirement semigroup {t, combine}
        deferred type :: t
    end requirement

  contains
    
    subroutine test_template()
    end subroutine
    
  end module

! template_error_07c
module template_error_07c_m
    implicit none
    private
    public :: op_t

    requirement semigroup {t, combine}
        deferred type :: t
        deferred interface
            elemental function combine(x, y) result(combined)
                type(t), intent(in) :: x, y
                type(t) :: combined
            end function
        end interface
    end requirement
  
    requirement extended_semigroup {t, combine, sconcat, stimes}
        require :: semigroup {t, scombine}
        deferred interface
            pure function sconcat(list) result(combined)
                type(t), intent(in) :: list(:)
                type(t) :: combined
            end function
            elemental function stimes(n, a) result(repeated)
                integer, intent(in) :: n
                type(t), intent(in) :: a
                type(t) :: repeated
            end function
        end interface
    end requirement

  contains
    
    subroutine test_template()
    end subroutine
    
  end module

! template_error_08
module template_add_01b_m_e
    implicit none
    private
    public :: add_t

    requirement R {T, F}
        deferred type :: T
        deferred interface
            function F(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, F}
        require :: R {T, F}
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = x + y
        end function
    end template

contains

    integer function func_arg_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    subroutine test_template()
        real :: a
        integer :: n, s

        instantiate add_t {integer, func_arg_int}, only: add_integer => add_generic
        n = add_integer(5, 9)
        !s = add_integer2(5, 9, 10)
        print*, "The result is", n
    end subroutine
end module

! template_error_07
module template_error_07_m
    implicit none
    private
    public :: tmp

    requirement r {t, f}
        deferred type :: t
        deferred interface
            function f(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
    end requirement

    template tmp {t}
        deferred type :: t
        require :: r {t}
    end template

contains
  
end module

! template_error_09
module std_prop_m
    implicit none
    public

    requirement magma_r {T, bin}
        deferred type :: T
        deferred interface
            pure elemental function bin(x, y) result(bin)
                type(T), intent(in) :: x
                type(T), intent(in) :: y
                type(T) :: bin
            end function
        end interface
    end requirement

    template commutative_prop {T,bin}
        require :: magma_r {T,bin}
      contains
        pure function commutative_p(x, y) result(prop)
            type(T), intent(in) :: x, y
            type(logical) :: prop

            prop = bin(x,y) == bin(y,x)
        end function
    end template
end module std_prop_m

! instantiate_type_arg_01
! An INSTANTIATE statement whose argument for a deferred type names something
! that is not declared. C1627 requires such an argument to specify an intrinsic
! type or a previously defined nonintrinsic type; `no_such_type` is neither.
! This used to dereference the unresolved symbol and segfault.

module instantiate_type_arg_01_mod
    implicit none

    template tmpl {t}
        deferred type :: t
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

end module instantiate_type_arg_01_mod

! continue_compilation_instantiate_01
! Erroneous INSTANTIATE statements under --continue-compilation.
!
! Each of the three statements below is reported cleanly without the flag. With
! it, the symbol table visitor reports the error and keeps going, leaving the
! symbols the body visitor would instantiate uncreated; the body visitor then
! used to hand those nulls to down_cast and to instantiate_body and crash.
!
! The requirement and the template here are deliberately valid, and this file
! deliberately contains no other kind of error, so that only the instantiate
! statements are under test. See
! tests/errors/continue_compilation_templates_01.f90 for errors in the
! requirement itself.

module continue_compilation_instantiate_01_mod
    implicit none

    requirement add_r {T, op}
        deferred type :: T
        deferred interface
            function op(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, op}
        require add_r {T, op}
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = op(x, y)
        end function
    end template

contains

    integer function add_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

end module continue_compilation_instantiate_01_mod

! continue_compilation_instantiate_02
! Erroneous instantiation arguments under --continue-compilation.
!
! Each statement below names something that cannot be an instantiation
! argument for the corresponding template parameter. The symbol table visitor
! used to dereference the resolved symbol without checking it, so an
! undeclared name segfaulted and a symbol of the wrong kind tripped an
! assertion inside ASRUtils::symbol_type.
!
! tests/errors/continue_compilation_instantiate_01.f90 covers erroneous
! template names and argument counts; this file covers the arguments
! themselves.

module continue_compilation_instantiate_02_mod
    implicit none

    template type_tmpl {t}
        deferred type :: t
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

    template const_tmpl {t, n}
        deferred type :: t
        deferred integer, parameter :: n
    contains
        subroutine sn(x)
            type(t), intent(in) :: x
            integer :: i
            do i = 1, n
                print *, i
            end do
        end subroutine
    end template

contains

    subroutine helper()
    end subroutine

end module continue_compilation_instantiate_02_mod

! instantiate_kwargs_01
! Keyword instantiation arguments (R1630) that break the correspondence rules
! of 16.5.5.1 para 2.
!
! The requirement and the template here are deliberately valid, and every
! instantiate statement below is wrong only in how its arguments correspond to
! the template's deferred arguments, so that nothing else is under test.

module instantiate_kwargs_01_mod
    implicit none

    requirement add_r {T, op}
        deferred type :: T
        deferred interface
            function op(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t {T, op}
        require add_r {T, op}
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = op(x, y)
        end function
    end template

contains

    integer function add_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

end module instantiate_kwargs_01_mod

! instantiate_type_arg_01
module instantiate_type_arg_01_checks
    use instantiate_type_arg_01_mod
    implicit none

    instantiate tmpl {no_such_type}, only: s
end module instantiate_type_arg_01_checks

! continue_compilation_instantiate_01
module continue_compilation_instantiate_01_checks
    use continue_compilation_instantiate_01_mod
    implicit none

    ! Unknown template name
    instantiate add_unknown_t {integer, add_int}, only: add1 => add_generic

    ! Unknown instantiation argument
    instantiate add_t {integer, add_unknown}, only: add2 => add_generic

    ! Wrong number of instantiation arguments
    instantiate add_t {integer}, only: add3 => add_generic

end module continue_compilation_instantiate_01_checks

! continue_compilation_instantiate_02
module continue_compilation_instantiate_02_checks
    use continue_compilation_instantiate_02_mod
    implicit none

    ! Undeclared name as the argument for a deferred type
    instantiate type_tmpl {no_such_type}, only: s1 => s

    ! A subroutine is not a type
    instantiate type_tmpl {helper}, only: s2 => s

    ! Undeclared name as the argument for a non-type parameter
    instantiate const_tmpl {integer, no_such_n}, only: s3 => sn
end module continue_compilation_instantiate_02_checks

! instantiate_kwargs_01
module instantiate_kwargs_01_checks
    use instantiate_kwargs_01_mod
    implicit none

    ! C1625: a positional argument cannot follow a keyword one
    instantiate add_t {op = add_int, integer}, only: add1 => add_generic

    ! C1626: `typ` is not the name of a deferred argument
    instantiate add_t {typ = integer, op = add_int}, only: add2 => add_generic

    ! Two instantiation arguments correspond to the deferred argument `t`
    instantiate add_t {T = integer, T = integer}, only: add3 => add_generic

    ! No instantiation argument corresponds to the deferred argument `op`
    instantiate add_t {T = integer}, only: add4 => add_generic

end module instantiate_kwargs_01_checks


! deferred_const_decl_1
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
!           lower bound. The lower bound clause is not diagnosed separately;
!           see the note on c1621_lbound below.
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

    ! C1621 forbids an explicit lower bound, in any of its spellings. That
    ! clause is not diagnosed on its own: `array_comp_decl` synthesizes the
    ! implicit lower bound, so `(3)` and `(1:3)` are the same by the time the
    ! semantic stage sees them. Both are still rejected, as an array deferred
    ! constant is not implemented, which is what these two pin.
    requirement c1621_lbound {g}
        deferred integer, parameter :: g(1:3)  ! {Error} a `deferred` constant that is an array is not supported yet
    end requirement

    requirement c1621_lbound_star {h}
        deferred integer, parameter :: h(2:*)  ! {Error} a `deferred` constant that is an array is not supported yet
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
    template note2 {x2, x3, x4, x5, x6, x7}
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

! deferred_type_duplicate_1
! Declaring the same deferred type name twice inside a requirement.
!
! The deferred-type path of SymbolTableVisitor::visit_DerivedType added the
! symbol unconditionally, and SymbolTable::add_symbol asserts the name is still
! free, so this invalid input reached an internal assertion instead of a
! diagnostic. In a Release build, where the assertion is compiled out, the
! duplicate silently overwrote the first declaration.
!
! A template declaring a duplicate deferred type goes through the very same
! branch of the same function; see integration_tests/template_07.f90 for the
! accepted spellings.

module deferred_type_duplicate_1
    implicit none

    requirement r {t}
        deferred type :: t
        deferred type :: t  ! {Error} Symbol is already declared in the same scope
    end requirement

end module

! template_end_name_1
! The optional construct name on an end statement of a TEMPLATE or a
! REQUIREMENT construct must match the name on the opening statement
! (J3/26-007r1, 16.2 and 16.6):
!
!     R1603 end-template-stmt     is  END TEMPLATE [ template-name ]
!     C1602 If a template-name appears in an end-template-stmt, it shall be
!           the same as that in the corresponding template-stmt.
!
!     R1635 end-requirement-stmt  is  END REQUIREMENT [ requirement-name ]
!     C1638 If a requirement-name appears in the end-requirement-stmt, it
!           shall be the same as that in the corresponding requirement-stmt.
!
! See integration_tests/template_end_name_01.f90 for the accepted spellings.

module template_end_name_1

    requirement r {t}
        deferred type :: t
    end requirement not_r  ! {Error} End requirement name does not match requirement name

    template tmpl {u}
        deferred type :: u
    end template not_tmpl  ! {Error} End template name does not match template name

end module

! instantiate_syntax_1
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

    template tmpl {t}
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

! require_syntax_1
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


! deferred_interface_scope_1
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

    requirement r {t, f}
        deferred type :: t
        deferred interface
            procedure f  ! {Error} a deferred interface block can only contain interface bodies
        end interface
    end requirement

end module

! deferred_proc_decl_1
module deferred_proc_decl_1_mod
    implicit none

    abstract interface
        subroutine iface(x)
            integer, intent(in) :: x
        end subroutine
    end interface

    integer :: not_an_interface

    requirement r1 {p}
        deferred procedure (iface) :: p, q  ! {Error} 'q' is not a deferred argument of 'r1'
    end requirement

    requirement r2 {p}
        deferred procedure (no_such_iface) :: p  ! {Error} the interface 'no_such_iface' is not declared
    end requirement

    template t1 {p}
        deferred procedure (not_an_interface) :: p  ! {Error} 'not_an_interface' is not an interface
    end template

    ! A module has no deferred arguments.
    deferred procedure (iface) :: p  ! {Error} a deferred procedure can only be declared in a requirement, a template or a templated procedure

end module

! deferred_type_scope_1
module deferred_type_scope_1_mod
    implicit none

    ! A module has no deferred arguments.
    deferred type :: t  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure

contains

    subroutine s()
        deferred type :: u  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure
    end subroutine

end module

module deferred_type_scope_1_args_mod
    implicit none

    requirement r {t}
        deferred type :: t
        deferred type :: y  ! {Error} 'y' is not a deferred argument of 'r'
    end requirement

    template tmpl {t}
        deferred type :: t
        deferred type :: z  ! {Error} 'z' is not a deferred argument of 'tmpl'
    end template

contains

    template subroutine swap{t}(x, y)
        deferred type :: t
        deferred type :: w  ! {Error} 'w' is not a deferred argument of 'swap'
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

end module


! deferred_type_attr_1
! Constraints on the deferred-type-attrs of a `deferred type` declaration
! (Fortran 2028 working draft J3/26-007r1):
!
!   R1617  deferred-type-attr  is  ABSTRACT
!                              or  EXTENSIBLE
!   C1614  A deferred-type-attr-list shall not specify both ABSTRACT and
!          EXTENSIBLE.
!   C1615  A deferred-type-attr-list shall contain at most one of each
!          deferred-type-attr.
!   C1616  The name of a deferred type shall not appear as a parent-type-name
!          in a type-attr-spec.
!   C706   In a declaration-type-spec, CLASS ( derived-type-spec ) or
!          CLASS ( deferred-type-name ) shall specify an extensible type.
!   C707   In a declaration-type-spec, TYPE(derived-type-spec) or
!          TYPE ( deferred-type-name ) shall not specify an abstract type.
!   C1628  A type-spec that is a instantiation-arg shall specify an extensible
!          type if its corresponding deferred type has the EXTENSIBLE
!          attribute. It shall not specify an abstract type unless its
!          corresponding deferred type has the ABSTRACT attribute.
!
! The accepted spellings are in
! integration_tests/template_deferred_type_attr_01.f90.

module deferred_type_attr_1
    implicit none

    type, abstract :: abstract_t
        integer :: i
    end type

    type, bind(c) :: bindc_t
        integer :: i
    end type

    requirement r_both {t}
        deferred type, abstract, extensible :: t  ! {Error} a deferred type cannot be declared both abstract and extensible
    end requirement

    requirement r_twice_abstract {t}
        deferred type, abstract, abstract :: t  ! {Error} the 'abstract' attribute is repeated in a deferred type declaration
    end requirement

    requirement r_twice_extensible {t}
        deferred type, extensible, extensible :: t  ! {Error} the 'extensible' attribute is repeated in a deferred type declaration
    end requirement

    requirement r_extended {t}
        deferred type, extensible :: t
        type, extends(t) :: u  ! {Error} deferred type 't' cannot be extended
            integer :: i
        end type
    end requirement

    requirement r_class_plain {t, f}
        deferred type :: t
        deferred interface
            function f(x) result(z)
                class(t), intent(in) :: x  ! {Error} deferred type 't' is not extensible, so it cannot be used in a class declaration
                integer :: z
            end function
        end interface
    end requirement

    requirement r_type_abstract {t, f}
        deferred type, abstract :: t
        deferred interface
            function f(x) result(z)
                type(t), intent(in) :: x  ! {Error} deferred type 't' is abstract, so it cannot be used in a type declaration
                integer :: z
            end function
        end interface
    end requirement

    template t_extensible {t}
        deferred type, extensible :: t
        private
        public :: nothing_ext
    contains
        subroutine nothing_ext()
        end subroutine
    end template

    template t_abstract {t}
        deferred type, abstract :: t
        private
        public :: nothing_abs
    contains
        subroutine nothing_abs()
        end subroutine
    end template

    template t_plain {t}
        deferred type :: t
        private
        public :: nothing_plain
    contains
        subroutine nothing_plain()
        end subroutine
    end template

contains

    subroutine bad_instantiations()
        ! C1628, first sentence: an intrinsic type is not extensible (7.5.7).
        instantiate t_extensible {integer}, only: n1 => nothing_ext  ! {Error} deferred type 't' is extensible, so its instantiation argument must be an extensible derived type, not integer
        ! C1628, first sentence: a BIND(C) derived type is not extensible.
        instantiate t_abstract {bindc_t}, only: n2 => nothing_abs  ! {Error} deferred type 't' is extensible, so its instantiation argument must be an extensible derived type, not bindc_t
        ! C1628, second sentence: only an ABSTRACT deferred type accepts an
        ! abstract instantiation argument.
        instantiate t_extensible {abstract_t}, only: n3 => nothing_ext  ! {Error} deferred type 't' is not abstract, so its instantiation argument must not be the abstract type abstract_t
        instantiate t_plain {abstract_t}, only: n4 => nothing_plain  ! {Error} deferred type 't' is not abstract, so its instantiation argument must not be the abstract type abstract_t
    end subroutine

end module

! template_save_common_1
! C1610 and C1611 (J3/26-007r1, 16.3): within a template or templated
! procedure, or a scoping unit nested therein, an entity that is not accessed
! by host or use association shall not have the SAVE attribute, and a COMMON or
! EQUIVALENCE statement shall not appear.
module template_save_common_1_m
    implicit none

    ! C1610, in a procedure of a template, in each spelling of SAVE
    template save_tmpl {t}
        deferred type :: t
    contains
        subroutine explicit_save(x)
            type(t), intent(in) :: x
            integer, save :: counter
            counter = counter + 1
            print *, x, counter
        end subroutine

        subroutine save_statement(x)
            type(t), intent(in) :: x
            integer :: counter
            save :: counter
            counter = counter + 1
            print *, x, counter
        end subroutine

        subroutine bare_save_statement(x)
            type(t), intent(in) :: x
            integer :: counter
            save
            counter = counter + 1
            print *, x, counter
        end subroutine

        ! An initialized local has an implicit SAVE attribute
        subroutine initialized_local(x)
            type(t), intent(in) :: x
            integer :: counter = 0
            counter = counter + 1
            print *, x, counter
        end subroutine

        ! A procedure contained in a procedure of a template is a scoping unit
        ! nested in the template
        subroutine nested_procedure(x)
            type(t), intent(in) :: x
            print *, x
            call inner()
        contains
            subroutine inner()
                integer, save :: counter
                counter = counter + 1
                print *, counter
            end subroutine
        end subroutine
    end template

    ! C1610 is not reachable in the specification part of a template itself.
    ! C1603 allows only a PARAMETER declaration there, and a named constant
    ! cannot have the SAVE attribute, so such a declaration is rejected as a
    ! C1603 violation before SAVE is ever considered. C1610 is still reachable,
    ! and is covered above and below, in a procedure of a template's CONTAINS
    ! section, in a scoping unit nested in one, and in a templated procedure.

    ! C1610, in a template nested in a template
    template outer_tmpl {t}
        deferred type :: t
        template inner_tmpl {u}
            deferred type :: u
        contains
            subroutine inner_save(y)
                type(u), intent(in) :: y
                integer, save :: counter
                counter = counter + 1
                print *, y, counter
            end subroutine
        end template
    contains
        subroutine outer_sub(x)
            type(t), intent(in) :: x
            print *, x
        end subroutine
    end template

    ! C1611, in a procedure of a template
    template storage_tmpl {t}
        deferred type :: t
    contains
        subroutine common_block(x)
            type(t), intent(in) :: x
            integer :: a
            common /blk/ a
            a = 1
            print *, x, a
        end subroutine

        subroutine equivalenced(x)
            type(t), intent(in) :: x
            integer :: a, b
            equivalence (a, b)
            a = 1
            print *, x, b
        end subroutine
    end template

contains

    ! C1610, in a templated procedure
    template subroutine templated_save{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        integer, save :: counter
        counter = counter + 1
        print *, x, counter
    end subroutine

    ! C1611, in a templated procedure
    template subroutine templated_common{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        integer :: a
        common /blk2/ a
        a = 1
        print *, x, a
    end subroutine

end module

! template_scope_1
! C1601 of the Fortran 2028 working draft (J3/26-007r1, 16.1.1):
!
!     A template-construct shall only appear in the specification-part of a
!     main program, module, or TEMPLATE construct.
!
! LFortran used to accept a template construct in any specification part, in
! particular inside an ordinary subprogram. A submodule is deliberately not in
! the list above, so a template in a submodule is rejected as well.
!
! See integration_tests/template_simple_01.f90 for a template in a module,
! integration_tests/template_program_01.f90 for one in a main program and
! integration_tests/template_05.f90 for a template nested in another template.

module template_scope_1_mod
    implicit none

    ! A requirement is not one of the three permitted contexts.
    requirement r {t}
        deferred type :: t
        template req_tmpl {u}  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
    end requirement

contains

    subroutine s()
        template sub_tmpl {u}  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
    end subroutine

    integer function f()
        template func_tmpl {u}  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
        f = 1
    end function

end module

module template_scope_1_submod_mod
    implicit none
    interface
        module subroutine g()
        end subroutine
    end interface
end module

submodule (template_scope_1_submod_mod) template_scope_1_submod
    implicit none

    ! A submodule is not a module for the purposes of C1601.
    template submod_tmpl {u}  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
        deferred type :: u
    end template

contains

    module subroutine g()
    end subroutine

end submodule

! deferred_type_class_1
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
        deferred interface
            subroutine s1(x)
                class(t), intent(in) :: x  ! {Error} deferred type 't' is not extensible, so it cannot be used in a class declaration
            end subroutine
        end interface
    end requirement

    template tmpl {u}
        deferred type :: u
    contains
        subroutine s2(y)
            class(u), intent(in) :: y  ! {Error} deferred type 'u' is not extensible, so it cannot be used in a class declaration
        end subroutine
    end template

end module

! deferred_type_coarray_1
! C1617 of the Fortran 2028 working draft (J3/26-007r1, 16.4.1.2):
!
!     C1617  A variable of deferred type shall not be a coarray.
!
! NOTE 5 of the same subclause gives the reason: it is invalid to coindex a
! variable that has a polymorphic potential subobject component, and a type
! with such a component is a permitted instantiation argument. The constraint
! therefore has to be checked on the declaration inside the template, once,
! rather than on each instantiation argument.
!
! LFortran used to accept every spelling below without a diagnostic; this test
! pins that each of them is now a semantic error. See
! integration_tests/template_deferred_type_01.f90 for accepted declarations of
! a deferred type, and `ordinary_coarray` here for a coarray of an ordinary
! type inside a template, which stays valid.

module deferred_type_coarray_1

    implicit none

    type :: ordinary_t
        integer :: a
    end type

    ! Specification part of a template.
    template spec_tmpl {t}
        deferred type :: t
    contains
        subroutine spec_coarray()
        type(t), codimension[:], allocatable :: spec_x  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
    end template

    ! Contains part of a template, all coarray spellings.
    template body_tmpl {t}
        deferred type :: t
    contains
        subroutine codim_attr()
            type(t), codimension[:], allocatable :: x  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
        subroutine bracket_allocatable()
            type(t), allocatable :: x[:]  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
        subroutine bracket_explicit()
            type(t) :: x[*]  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
        subroutine codim_attr_explicit()
            type(t), codimension[*] :: x  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
        subroutine with_dimension()
            type(t), dimension(:), codimension[:], allocatable :: x  ! {Error} A variable of deferred type must not be a coarray
        end subroutine
        ! A coarray of an ordinary type inside a template is unaffected by
        ! C1617 and must keep compiling.
        subroutine ordinary_coarray()
            type(ordinary_t), codimension[:], allocatable :: y
        end subroutine
    end template

    ! Specification part of a requirement (R1634 rejects it before C1617).
    requirement r {t}
        deferred type :: t
        type(t), codimension[:], allocatable :: req_x  ! {Error} 'req_x' is not a deferred argument of 'r'
    end requirement

contains

    ! Brace-spelled templated subprogram.
    template subroutine templated_sub{t}()
        deferred type :: t
        type(t), codimension[:], allocatable :: x  ! {Error} A variable of deferred type must not be a coarray
    end subroutine

end module deferred_type_coarray_1

! template_spec_decl_1
! C1603 and C1604 of the Fortran 2028 working draft (J3/26-007r1, 16.1.1):
!
!     C1603  If a template-specification is a type declaration statement, it
!            shall specify the PARAMETER attribute.
!     C1604  If a template-specification is a procedure declaration statement,
!            it shall not specify the POINTER attribute.
!
! with the accompanying note: a template specification part cannot declare a
! variable or procedure pointer.
!
! Both constraints apply to the template-specifications only, that is to the
! items between the `template` statement and `contains`. The contained
! procedures are ordinary subprogram bodies and may declare locals, and a
! deferred argument declaration (R1615) or a requirement is not a
! template-specification either.

module template_spec_decl_1_mod
    implicit none

    abstract interface
        subroutine iface_sub()
        end subroutine
    end interface

    requirement plus_r {t, plus_t}
        deferred type :: t
        deferred interface
            function plus_t(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
    end requirement

    template tmpl {t, plus_t, n}
        ! Not template-specifications, and so not restricted by C1603: a
        ! deferred type declaration, a deferred constant and a requirement.
        deferred type :: t
        deferred integer, parameter :: n
        require :: plus_r {t, plus_t}

        ! A named constant is what C1603 permits.
        integer, parameter :: repeat_count = 2

        ! A procedure declaration without POINTER is what C1604 permits.
        procedure(iface_sub) :: plain_proc

        private
        public :: add_n_times

        integer :: bad_variable          ! {Error} a template specification part cannot declare a variable, so 'bad_variable' must have the parameter attribute
        procedure(iface_sub), pointer :: bad_proc_ptr          ! {Error} a template specification part cannot declare a procedure pointer, so 'bad_proc_ptr' must not have the pointer attribute
    contains
        function add_n_times(x) result(z)
            type(t), intent(in) :: x
            type(t) :: z
            ! A local variable in a contained procedure stays legal.
            integer :: i
            z = x
            do i = 1, n * repeat_count
                z = plus_t(z, x)
            end do
        end function
    end template

end module

! requirement_scope_1
! The Fortran 2028 working draft (J3/26-007r1) contradicts itself about where a
! REQUIREMENT construct may appear:
!
!     R1605 template-declaration  is  template-specification
!                                 or  deferred-arg-decl-stmt
!                                 or  requirement-construct
!                                 or  template-construct
!
!     C1636 A requirement-construct shall only appear in the specification-part
!           of a main program or module.
!
! R1605 permits a requirement construct inside a TEMPLATE construct, C1636
! forbids it. This is a drafting defect, not a settled rule; LFortran follows
! C1636 because rejecting is reversible, while accepting code the standard may
! forbid creates a compatibility burden if J3 resolves it the other way.
!
! A submodule, a subprogram and another requirement are not in C1636's list
! either, so a requirement is rejected in those as well.
!
! See integration_tests/template_simple_01.f90 for a requirement in a module and
! integration_tests/template_03b.f90 for one in a main program.

module requirement_scope_1_mod
    implicit none

    ! Permitted: the specification part of a module.
    requirement ok_r {t}
        deferred type :: t
    end requirement

    ! R1605 would allow this, C1636 does not.
    template tmpl {t}
        deferred type :: t
        requirement tmpl_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

    ! A requirement is not a main program or a module either.
    requirement outer_r {t}
        deferred type :: t
        requirement inner_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    end requirement

contains

    subroutine sub()
        requirement sub_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    end subroutine

    integer function func()
        requirement func_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
        func = 1
    end function

end module

module requirement_scope_1_submod_mod
    implicit none
    interface
        module subroutine g()
        end subroutine
    end interface
end module

submodule (requirement_scope_1_submod_mod) requirement_scope_1_submod
    implicit none

    ! A submodule is not a module for the purposes of C1636.
    requirement submod_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
        deferred type :: u
    end requirement

contains

    module subroutine g()
    end subroutine

end submodule

! requirement_syntax_1
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
        deferred type :: u
        require :: r(u)  ! {Error} Token '(' is unexpected here
    end template

    ! Placed last: the requirement statement opens a construct, so its syntax
    ! error also leaves the matching END REQUIREMENT unexpected.
    requirement r_paren(t)  ! {Error} Token '(' is unexpected here
        integer :: t
    end requirement  ! {Error} Token 'requirement' is unexpected here

end module

! requirement_bare_subprogram_1
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

! deferred_type_syntax_1
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
        deferred type :: t
        type, deferred :: t  ! {Error} Token 'deferred' is unexpected here
    end requirement

    template tmpl {u}
        deferred type :: u
        type, deferred :: u  ! {Error} Token 'deferred' is unexpected here
    end template

    ! Outside a template or a requirement it was never meaningful either.
    type, deferred :: v  ! {Error} Token 'deferred' is unexpected here

end module

! decl_order_01
module decl_order_mod
implicit none
integer :: i
i = 1

! A template accepts declarations only
template decl_order_t {T}
    deferred type :: T
    use iso_fortran_env
    implicit none
    integer, parameter :: j = 0
    j = 1
end template
end module

subroutine decl_order_sub()
implicit none
integer :: a
import :: y
a = 1
end subroutine


! C1637 (J3/26-007r1): the interface-stmt of an interface block that is a
! requirement-specification shall specify ABSTRACT or DEFERRED. A plain
! interface block declares an external procedure with an explicit interface,
! and a generic one builds a generic set out of procedures declared
! elsewhere; neither declares a deferred argument.
module requirement_interface_kind_1
    implicit none

    requirement plain_interface_req {T2, plain_func}
        deferred type :: T2
        interface  ! {Error} an interface block in a requirement must be a deferred or an abstract interface
            function plain_func(x) result(y)
                type(T2), intent(in) :: x
                type(T2) :: y
            end function
        end interface
    end requirement

    ! A second one, to show compilation continues past the first.
    requirement operator_interface_req {U, plus_u}
        deferred type :: U
        interface operator (+)  ! {Error} an interface block in a requirement must be a deferred or an abstract interface
            procedure plus_u
        end interface
        deferred interface
            function plus_u(x, y) result(z)
                type(U), intent(in) :: x, y
                type(U) :: z
            end function
        end interface
    end requirement

end module

! A deferred constant is declared by a deferred-const-decl-stmt (R1618). A
! plain type declaration of a deferred argument used to mean the same thing in
! LFortran; it is a C1603 violation and is now rejected.
module template_plain_const_1
    implicit none

    template plain_const_tmpl {T3, n3}
        deferred type :: T3
        integer :: n3  ! {Error} 'n3' is a deferred argument of the template, so a type declaration of it declares a deferred constant, which is spelled `deferred <type>, parameter :: n3`
    end template

    ! The same for an array deferred constant, to show the message does not
    ! depend on the entity's shape.
    template plain_const_array_tmpl {T4, n4}
        deferred type :: T4
        integer :: n4(3)  ! {Error} 'n4' is a deferred argument of the template, so a type declaration of it declares a deferred constant, which is spelled `deferred <type>, parameter :: n4`
    end template

end module

! A templated subprogram carries TEMPLATE in its prefix (C1609), with the
! deferred argument list in braces as J3 paper 26-158 corrects R1611 and R1612.
! LFortran used to accept the deferred argument list with no TEMPLATE in the
! prefix at all; one case per removed grammar production. See
! integration_tests/template_prefix_01.f90 for the accepted spellings.
module templated_subp_syntax_1a
    implicit none
contains
    subroutine swap{t}(x, y)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(inout) :: x, y
    end subroutine
end module

! Each case is its own module so that recovery from the previous syntax error
! does not swallow the next statement under test.
module templated_subp_syntax_1b
    implicit none
contains
    pure subroutine copy_into{t}(x, y)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
    end subroutine
end module

module templated_subp_syntax_1c
    implicit none
contains
    function pick_last{t}(x, y) result(res)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
    end function
end module

module templated_subp_syntax_1d
    implicit none
contains
    pure function pick_second{t}(x, y) result(res)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
    end function
end module

! A TEMPLATE statement spells its deferred argument list with braces (R1602 as
! corrected by 26-158), so the parenthesised spelling is now a syntax error,
! with and without arguments.
module template_syntax_1
    implicit none

    requirement paren_r {t}
        deferred type :: t
    end requirement

    template one_arg_tmpl(u)  ! {Error} Token '(' is unexpected here
        deferred type :: u
        require :: paren_r {u}
    end template

    template no_arg_tmpl()  ! {Error} Token '(' is unexpected here
        integer, parameter :: n = 1
    end template

end module

program continue_compilation_templates_01
    use continue_compilation_templates_01_mod
    implicit none

    ! deferred_interface_scope_1
    deferred interface  ! {Error} a deferred interface can only appear in a requirement, a template or a templated procedure
        subroutine s(x)
            integer, intent(in) :: x
        end subroutine
    end interface

    ! deferred_proc_decl_1
    deferred procedure (iface) q  ! {Error} a deferred procedure can only be declared in a requirement, a template or a templated procedure

    ! deferred_type_scope_1
    deferred type :: v  ! {Error} a deferred type can only be declared in a requirement, a template or a templated procedure

    ! decl_order_01 main-program cases
import :: x
integer :: b
use, intrinsic :: iso_c_binding
implicit none
block
implicit none
integer :: c
c = 1
end block

end program continue_compilation_templates_01

! A requirement specification holds only deferred argument declarations and
! interface blocks (R1634), so a plain type declaration of one of its
! arguments is rejected where it is written instead of being taken for a
! deferred type (#13328).
module requirement_non_deferred_decl_1
    implicit none

    requirement r_derived {t, c}
        deferred type :: t
        type(t) :: c  ! {Error} 'c' is a deferred argument of requirement 'r_derived', so it must be declared as a deferred type, a deferred constant or a deferred procedure
    end requirement

    requirement r_intrinsic {c}
        integer :: c  ! {Error} 'c' is a deferred argument of requirement 'r_intrinsic', so it must be declared as a deferred type, a deferred constant or a deferred procedure
    end requirement

    requirement r_not_arg {t}
        deferred type :: t
        real :: x  ! {Error} 'x' is not a deferred argument of 'r_not_arg'
    end requirement

end module
