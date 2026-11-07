! An abstract extension declared in another module has a new DEFERRED binding
! `p` named like the parent's PRIVATE (non-deferred) binding `p` (F2023
! 7.5.7.3): it overrides nothing and gets a vtable slot of its own, so the
! leaf's implementation, with an extra argument, does not replace the parent's
! `p` for calls in the parent's module. GFortran treats the new binding as an
! override (and rejects the extra argument).
module type_bound_private_11_base
    implicit none
    private
    public :: base_t, run
    type :: base_t
    contains
        procedure, private :: p => base_p
    end type
contains
    integer function base_p(this)
        class(base_t), intent(in) :: this
        base_p = 1
    end function
    integer function run(b)
        class(base_t), intent(in) :: b
        run = b%p()
    end function
end module
module type_bound_private_11_child
    use type_bound_private_11_base
    implicit none
    type, abstract, extends(base_t) :: mid_t
    contains
        procedure(iface), deferred :: p
    end type
    abstract interface
        integer function iface(this, k)
            import :: mid_t
            class(mid_t), intent(in) :: this
            integer, intent(in) :: k
        end function
    end interface
    type, extends(mid_t) :: leaf_t
    contains
        procedure :: p => leaf_p
    end type
contains
    integer function leaf_p(this, k)
        class(leaf_t), intent(in) :: this
        integer, intent(in) :: k
        leaf_p = 100 + k
    end function
end module
program type_bound_private_11
    use type_bound_private_11_base
    use type_bound_private_11_child
    implicit none
    type(leaf_t) :: x
    class(base_t), allocatable :: b
    class(mid_t), allocatable :: m
    allocate(leaf_t :: b, m)
    print *, run(x), run(b), x%p(5), m%p(6)
    if (run(x) /= 1) error stop 1
    if (run(b) /= 1) error stop 2
    if (x%p(5) /= 105) error stop 3
    if (m%p(6) /= 106) error stop 4
    print *, "ok"
end program
