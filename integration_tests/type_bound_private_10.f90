! A PRIVATE DEFERRED binding of an abstract type, implemented by a binding of
! the same name in an extension declared in another module (#7018). The
! private binding is not accessible there, but an abstract type could then
! never be completed outside its module, so the same-named binding overrides
! it, as in GFortran: calls through the generic and through dispatch in the
! parent's module reach the implementation.
module type_bound_private_10_base
    implicit none
    private
    public :: abc_t, base_t, plain_t, run_base, run_plain, run_write
    type, abstract :: abc_t
    contains
        procedure(print_iface), deferred, private :: print
        generic, public :: write => print
    end type
    type, abstract :: base_t
    contains
        procedure(p_iface), deferred, private :: p
        generic, public :: g => p
    end type
    type, abstract :: plain_t
    contains
        procedure(q_iface), deferred, private :: q
    end type
    abstract interface
        subroutine print_iface(this, n)
            import :: abc_t
            class(abc_t), intent(inout) :: this
            integer, intent(out) :: n
        end subroutine
        integer function p_iface(this)
            import :: base_t
            class(base_t), intent(in) :: this
        end function
        integer function q_iface(this)
            import :: plain_t
            class(plain_t), intent(in) :: this
        end function
    end interface
contains
    integer function run_write(a)
        class(abc_t), intent(inout) :: a
        call a%print(run_write)
    end function
    integer function run_base(b)
        class(base_t), intent(in) :: b
        run_base = b%p()
    end function
    integer function run_plain(c)
        class(plain_t), intent(in) :: c
        run_plain = c%q()
    end function
end module

module type_bound_private_10_impl
    use type_bound_private_10_base, only: abc_t, base_t, plain_t
    implicit none
    private
    public :: example_t, child_t, plain_child_t
    type, extends(abc_t) :: example_t
    contains
        procedure, private :: print => print_example
    end type
    type, extends(base_t) :: child_t
    contains
        procedure :: p => child_p
    end type
    type, extends(plain_t) :: plain_child_t
    contains
        procedure :: q => plain_child_q
    end type
contains
    subroutine print_example(this, n)
        class(example_t), intent(inout) :: this
        integer, intent(out) :: n
        n = 7
    end subroutine
    integer function child_p(this)
        class(child_t), intent(in) :: this
        child_p = 42
    end function
    integer function plain_child_q(this)
        class(plain_child_t), intent(in) :: this
        plain_child_q = 13
    end function
end module

program type_bound_private_10
    use type_bound_private_10_base, only: abc_t, base_t, plain_t, &
        run_base, run_plain, run_write
    use type_bound_private_10_impl, only: example_t, child_t, plain_child_t
    implicit none
    type(example_t) :: e
    type(child_t) :: x
    type(plain_child_t) :: y
    class(abc_t), allocatable :: a
    class(base_t), allocatable :: b
    class(plain_t), allocatable :: c
    integer :: n
    allocate(example_t :: a)
    allocate(child_t :: b)
    allocate(plain_child_t :: c)
    n = 0
    call e%write(n)
    if (n /= 7) error stop 1
    n = 0
    call a%write(n)
    if (n /= 7) error stop 2
    if (run_write(e) /= 7) error stop 3
    if (run_write(a) /= 7) error stop 4
    if (x%g() /= 42) error stop 5
    if (x%p() /= 42) error stop 6
    if (b%g() /= 42) error stop 7
    if (run_base(x) /= 42) error stop 8
    if (run_base(b) /= 42) error stop 9
    if (y%q() /= 13) error stop 10
    if (run_plain(y) /= 13) error stop 11
    if (run_plain(c) /= 13) error stop 12
    print *, "ok"
end program
