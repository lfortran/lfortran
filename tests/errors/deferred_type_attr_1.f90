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
