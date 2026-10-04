! Test that allocate(source=) of a type with a finalizer gets debug
! locations with -g: in a block after other statements, and with a
! polymorphic source.
module finalization_15_mod
    implicit none
    type :: t
        integer :: v = 0
    contains
        final :: fin
    end type
contains
    subroutine fin(this)
        type(t), intent(inout) :: this
        this%v = -1
    end subroutine

    subroutine alloc_in_block(n)
        type(t), intent(in) :: n
        integer :: k
        k = n%v
        block
            type(t), pointer :: c
            allocate(c, source=n)
            if (c%v /= k) error stop
            deallocate(c)
        end block
    end subroutine

    subroutine alloc_class(n)
        class(t), intent(in) :: n
        class(t), allocatable :: c
        allocate(c, source=n)
        if (c%v /= 5) error stop
    end subroutine
end module

program finalization_15
    use finalization_15_mod
    implicit none
    type(t) :: x
    x%v = 5
    call alloc_in_block(x)
    call alloc_class(x)
    print *, "ok"
end program
