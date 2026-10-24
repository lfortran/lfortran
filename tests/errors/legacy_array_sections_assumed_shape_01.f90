! Sequence association applies only to explicit-shape and assumed-size dummy
! arguments, so even with --legacy-array-sections an array element cannot be
! passed to an assumed-shape, allocatable or pointer array dummy argument.
module legacy_array_sections_assumed_shape_01_mod
    implicit none
contains
    subroutine int_shape(x)
        integer, intent(in) :: x(:)
    end subroutine

    subroutine char_shape(x)
        character(len=*), intent(in) :: x(:)
    end subroutine

    integer function int_shape_f(x)
        integer, intent(in) :: x(:)
        int_shape_f = size(x)
    end function

    subroutine int_alloc(x)
        integer, allocatable, intent(inout) :: x(:)
    end subroutine

    subroutine int_ptr(x)
        integer, pointer, intent(inout) :: x(:)
    end subroutine

    subroutine int_explicit(x)
        integer, intent(in) :: x(3)
    end subroutine

    subroutine int_rank(x)
        integer, intent(in) :: x(..)
    end subroutine
end module

program legacy_array_sections_assumed_shape_01
    use legacy_array_sections_assumed_shape_01_mod
    implicit none
    integer :: c(5), n
    integer, allocatable :: ca(:)
    integer, pointer :: cp(:)
    character(len=3) :: d(4)
    allocate(ca(5), cp(5))
    call int_shape(c(2))  ! {Error} element of a fixed-size array, use-associated procedure
    call int_shape(ca(2))  ! {Error} element of an allocatable array, use-associated procedure
    call char_shape(d(2))  ! {Error} element of a character array, use-associated procedure
    n = int_shape_f(c(2))  ! {Error} function reference
    call int_alloc(ca(2))  ! {Error} allocatable dummy
    call int_ptr(cp(2))  ! {Error} pointer dummy
    call local_shape(c(2))  ! {Error} element of a fixed-size array, internal procedure
    call local_shape(ca(2))  ! {Error} element of an allocatable array, internal procedure
    call local_char(d(2))  ! {Error} element of a character array, internal procedure
    ! Valid: sequence association with an explicit-shape dummy, and a scalar
    ! actual argument for an assumed-rank dummy.
    call int_explicit(c(2))
    call int_rank(c(2))
contains
    subroutine local_shape(x)
        integer, intent(in) :: x(:)
    end subroutine

    subroutine local_char(x)
        character(len=3), intent(in) :: x(:)
    end subroutine
end program
