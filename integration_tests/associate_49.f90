program associate_49
implicit none
type :: t
    integer, allocatable :: x
end type
type :: w_t
    type(t), pointer :: ptr => null()
end type
type(w_t) :: w
allocate(w%ptr)
w%ptr%x = 42
call run(w)
deallocate(w%ptr)
contains
subroutine process(x)
    integer, optional, intent(in) :: x
    if (present(x)) then
        if (x /= 42) error stop
    end if
end subroutine
subroutine run(w)
    type(w_t), intent(inout) :: w
    associate(p => w%ptr)
        call process(p%x)
    end associate
end subroutine
end program
