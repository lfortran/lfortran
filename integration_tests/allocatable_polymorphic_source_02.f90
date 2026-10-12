program allocatable_polymorphic_source_02
    implicit none

    type :: base_t
        integer :: a = 1
    end type

    type, extends(base_t) :: child_t
        integer :: b = 5
    end type

    class(base_t), allocatable :: xs, c(:)

    allocate(child_t :: xs)
    select type (xs)
    type is (child_t)
        xs%b = 7
    end select

    ! A scalar polymorphic SOURCE gives the allocated array its dynamic type.
    allocate(c(2), source=xs)
    select type (c)
    type is (child_t)
        if (any(c%b /= 7)) error stop 1
        print *, c%b
    class default
        error stop 2
    end select

    print *, "ok"
end program
