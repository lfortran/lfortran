! An element of a pointer, assumed-shape or polymorphic array may not be
! followed in memory by the elements that follow it in the array, so it is
! not sequence associated with an explicit-shape or assumed-size array
! dummy argument (F2023 15.5.2.4 p14), even with --legacy-array-sections.
module legacy_array_sections_pointer_element_01_mod
    implicit none
    type :: t
        integer :: i
    end type

    type :: holder
        integer, pointer :: p(:) => null()
        integer :: v(4)
    end type
contains
    subroutine int_explicit(x)
        integer, intent(inout) :: x(2)
    end subroutine

    subroutine int_size(x)
        integer, intent(inout) :: x(*)
    end subroutine

    integer function int_explicit_f(x)
        integer, intent(in) :: x(2)
        int_explicit_f = x(1)
    end function

    subroutine t_explicit(x)
        type(t), intent(inout) :: x(2)
    end subroutine

    subroutine char_explicit(x)
        character(len=1), intent(in) :: x(2)
    end subroutine

    subroutine int_rank(x)
        integer, intent(in) :: x(..)
    end subroutine

    subroutine from_assumed_shape(a, c)
        integer, intent(inout) :: a(:)
        class(t), intent(inout) :: c(:)
        call int_explicit(a(2))  ! {Error} element of an assumed-shape array, explicit-shape dummy
        call int_size(a(2))  ! {Error} element of an assumed-shape array, assumed-size dummy
        call t_explicit(c(2))  ! {Error} element of a polymorphic assumed-shape array
    end subroutine
end module

program legacy_array_sections_pointer_element_01
    use legacy_array_sections_pointer_element_01_mod
    implicit none
    integer, pointer :: p(:,:)
    integer, pointer, contiguous :: q(:)
    class(t), allocatable :: ca(:)
    type(holder) :: h
    type(holder), pointer :: hp(:)
    character(len=1), pointer :: cp(:)
    integer :: i, n
    allocate(p(2,3), q(4), ca(3), h%p(4), hp(2), cp(4))
    p = reshape([(i, i = 1, 6)], [2, 3])
    call negate(p(2,1))  ! {Error} element of a pointer array, internal procedure
    call int_explicit(q(2))  ! {Error} element of a contiguous pointer array
    call int_size(q(2))  ! {Error} element of a pointer array, assumed-size dummy
    n = int_explicit_f(q(2))  ! {Error} function reference
    call t_explicit(ca(2))  ! {Error} element of an allocatable polymorphic array
    call int_explicit(h%p(2))  ! {Error} element of a pointer array component
    ! Valid: an element of a fixed-size component of an element of a pointer
    ! array, a character element (character sequence association), and a
    ! scalar actual argument for an assumed-rank dummy.
    call int_explicit(hp(1)%v(2))
    call char_explicit(cp(2))
    call int_rank(p(2,1))
contains
    subroutine negate(x)
        integer, intent(inout) :: x(2,2)
        x = -x
    end subroutine
end program
