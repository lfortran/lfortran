! An extension declared in another module has a new binding `p` named like
! the parent's PRIVATE binding `p` (F2023 7.5.7.3), and its own generic `g`,
! which extends the parent's public generic `g`, names the new binding. The
! inherited `g` then has both specifics: the parent's `p` (no argument) and
! the extension's `p` (one argument). GFortran treats the new binding as an
! override (and rejects the extra argument).
module type_bound_private_09_base
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
        call_p = b%p() + 10 * b%g()
    end function
end module

module type_bound_private_09_child
    use type_bound_private_09_base
    implicit none
    type, extends(base_t) :: child_t
    contains
        procedure :: p => child_p
        generic :: g => p
    end type
contains
    integer function child_p(this, k)
        class(child_t), intent(in) :: this
        integer, intent(in) :: k
        child_p = 20 + k
    end function
end module

program type_bound_private_09
    use type_bound_private_09_base
    use type_bound_private_09_child
    implicit none
    type(child_t) :: x
    class(child_t), allocatable :: c
    print *, call_p(x), x%g(), x%g(5), x%p(5)
    if (call_p(x) /= 11) error stop 1
    if (x%g() /= 1) error stop 2
    if (x%g(5) /= 25) error stop 3
    if (x%p(5) /= 25) error stop 4
    allocate(child_t :: c)
    if (c%g() /= 1 .or. c%g(5) /= 25 .or. c%p(7) /= 27) error stop 5
    if (call_p(c) /= 11) error stop 6
    print *, "ok"
end program
