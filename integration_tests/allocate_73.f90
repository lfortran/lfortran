module allocate_73_mod
    implicit none
    type :: t
        integer :: val = 0
        class(t), pointer :: p => null()
    contains
        final :: finalize_t
    end type
contains
    subroutine do_allocate(x, y)
        type(t), intent(inout) :: x
        class(t), intent(in) :: y
        allocate(x%p, source=y)
    end subroutine

    subroutine finalize_t(this)
        type(t), intent(inout) :: this
        this%val = -1
    end subroutine
end module

program allocate_73
    ! Regression test: allocate with source= on a class(t) pointer member
    ! with a finalizer subroutine. Previously caused an ICE due to swapped
    ! type arguments in deepcopy and type mismatch in call_struct_finalize_fn.
    use allocate_73_mod
    implicit none

    type(t) :: x
    class(t), allocatable :: y

    allocate(y)
    y%val = 42

    call do_allocate(x, y)

    if (x%p%val /= 42) error stop "x%p%val should be 42"

    ! Verify deep copy (not aliasing)
    y%val = 99
    if (x%p%val /= 42) error stop "shallow copy detected"

    print *, "PASS"
    deallocate(x%p)
    deallocate(y)
end program allocate_73
