! Tests the deferred-type-attrs of a deferred type declaration (Fortran 2028
! working draft J3/26-007r1, 16.4.1.2):
!
!   R1616  deferred-type-declaration-stmt  is  DEFERRED TYPE
!                                                [, deferred-type-attr-list ] ::
!                                                deferred-arg-name-list
!   R1617  deferred-type-attr  is  ABSTRACT
!                              or  EXTENSIBLE
!
! "A deferred type with the EXTENSIBLE attribute is an extensible type. A
! deferred type with the ABSTRACT attribute is an abstract type. A deferred type
! with the ABSTRACT attribute implicitly has the EXTENSIBLE attribute."
! (16.4.1.2 paragraph 2)
!
! Both attributes therefore make the deferred type extensible, so CLASS(t) is
! permitted for them (C706) and the instantiation argument has to be an
! extensible derived type (C1628); only the ABSTRACT one accepts an abstract
! instantiation argument. The rejected spellings are in
! tests/errors/deferred_type_attr_1.f90.

module template_deferred_type_attr_01_m
    implicit none
    private
    public :: shape_t, square_t, rect_t, test_deferred_type_attr

    type, abstract :: shape_t
        integer :: tag
    end type

    type, extends(shape_t) :: square_t
        real :: side
    end type

    type, extends(shape_t) :: rect_t
        real :: w
        real :: h
    end type

    ! `u` is extensible and not abstract, so its instantiation argument has to be
    ! an extensible, nonabstract derived type.
    requirement area_ext_r {u, area}
        deferred type, extensible :: u
        deferred interface
            pure function area(s) result(a)
                class(u), intent(in) :: s
                real :: a
            end function
        end interface
    end requirement

    ! `u` is abstract, and hence also extensible, so its instantiation argument
    ! has to be extensible and is permitted to be abstract.
    requirement area_abs_r {u, area}
        deferred type, abstract :: u
        deferred interface
            pure function area(s) result(a)
                class(u), intent(in) :: s
                real :: a
            end function
        end interface
    end requirement

    template scaled_ext_tmpl(t, area_t)
        deferred type, extensible :: t
        require :: area_ext_r {t, area_t}
        private
        public :: scaled_ext
    contains
        pure function scaled_ext(s) result(a)
            class(t), intent(in) :: s
            real :: a
            a = 2.0 * area_t(s)
        end function
    end template

    template scaled_abs_tmpl(t, area_t)
        deferred type, abstract :: t
        require :: area_abs_r {t, area_t}
        private
        public :: scaled_abs
    contains
        pure function scaled_abs(s) result(a)
            class(t), intent(in) :: s
            real :: a
            a = 3.0 * area_t(s)
        end function
    end template

contains

    pure function square_area(s) result(a)
        class(square_t), intent(in) :: s
        real :: a
        a = s%side * s%side
    end function

    ! Dispatches on the dynamic type of an abstract instantiation argument.
    pure function shape_area(s) result(a)
        class(shape_t), intent(in) :: s
        real :: a
        select type (s)
            type is (square_t)
                a = s%side * s%side
            type is (rect_t)
                a = s%w * s%h
            class default
                a = 0.0
        end select
    end function

    subroutine test_deferred_type_attr()
        instantiate scaled_ext_tmpl {square_t, square_area}, only: sq2 => scaled_ext
        instantiate scaled_abs_tmpl {shape_t, shape_area}, only: sh3 => scaled_abs
        type(square_t) :: sq
        type(rect_t) :: rc
        sq%tag = 1
        sq%side = 3.0
        rc%tag = 2
        rc%w = 2.0
        rc%h = 5.0
        print *, sq2(sq), sh3(sq), sh3(rc)
        if (abs(sq2(sq) - 18.0) > 1.0e-6) error stop
        if (abs(sh3(sq) - 27.0) > 1.0e-6) error stop
        if (abs(sh3(rc) - 30.0) > 1.0e-6) error stop
    end subroutine

end module

program template_deferred_type_attr_01
    use template_deferred_type_attr_01_m
    implicit none

    call test_deferred_type_attr()

end program
