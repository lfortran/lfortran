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
    template spec_tmpl(t)
        deferred type :: t
        type(t), codimension[:], allocatable :: spec_x  ! {Error} A variable of deferred type must not be a coarray
    end template

    ! Contains part of a template, all coarray spellings.
    template body_tmpl(t)
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

    ! Specification part of a requirement.
    requirement r {t}
        deferred type :: t
        type(t), codimension[:], allocatable :: req_x  ! {Error} A variable of deferred type must not be a coarray
    end requirement

contains

    ! Templated subprogram.
    template subroutine templated_sub{t}()
        deferred type :: t
        type(t), codimension[:], allocatable :: x  ! {Error} A variable of deferred type must not be a coarray
    end subroutine

end module deferred_type_coarray_1
