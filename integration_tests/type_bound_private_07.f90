! A PRIVATE binding of the parent that an intermediate extension, declared in
! another module, does not redefine stays private to the parent's module, even
! though the parent's public generic refers to it. A grandchild declared in the
! intermediate extension's module with a binding of the same name has a new
! binding that does not override it (F2023 7.5.7.3), so it may also have a
! different interface. GFortran treats the new binding as an override (and
! rejects the extra argument).
module type_bound_private_07_base
    implicit none
    private
    public :: base_t, call_p
    type :: base_t
    contains
        procedure, private :: p => base_p
        generic, public :: g => p
    end type
contains
    integer function base_p(this)
        class(base_t), intent(in) :: this
        base_p = 1
    end function

    integer function call_p(b)
        class(base_t), intent(in) :: b
        call_p = b%p()
    end function
end module

module type_bound_private_07_child
    use type_bound_private_07_base
    implicit none
    type, extends(base_t) :: child_t
    end type
    type, extends(child_t) :: gc_t
    contains
        procedure :: p => gc_p
    end type
    type, extends(child_t) :: gc_arg_t
    contains
        procedure :: p => gc_arg_p
    end type
contains
    integer function gc_p(this)
        class(gc_t), intent(in) :: this
        gc_p = 3
    end function

    integer function gc_arg_p(this, k)
        class(gc_arg_t), intent(in) :: this
        integer, intent(in) :: k
        gc_arg_p = 3 + k
    end function
end module

program type_bound_private_07
    use type_bound_private_07_base
    use type_bound_private_07_child
    implicit none
    type(gc_t) :: x
    type(gc_arg_t) :: y
    class(child_t), allocatable :: c

    print *, call_p(x), x%g(), x%p()
    if (call_p(x) /= 1) error stop 1
    if (x%g() /= 1) error stop 2
    if (x%p() /= 3) error stop 3

    print *, call_p(y), y%g(), y%p(4)
    if (call_p(y) /= 1) error stop 4
    if (y%g() /= 1) error stop 5
    if (y%p(4) /= 7) error stop 6

    allocate(gc_t :: c)
    if (c%g() /= 1) error stop 7
    if (call_p(c) /= 1) error stop 8
    deallocate(c)

    allocate(gc_arg_t :: c)
    if (c%g() /= 1) error stop 9
    if (call_p(c) /= 1) error stop 10
end program
