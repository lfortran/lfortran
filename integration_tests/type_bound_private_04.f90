! A binding of an extension type declared in another module, with the same
! name as a PRIVATE binding of the parent, does not override it: the parent's
! binding is not accessible there, so this is a new binding (F2023 7.5.7.3).
! Calls in the parent's module, by name or through a generic, still reach the
! parent's procedure for an object of the extension type, and the new binding
! may have a different interface. GFortran gets this wrong; Flang and ifx
! follow the standard.
module type_bound_private_04_base
    implicit none
    private
    public :: base_t, run, hit
    integer :: hit = 0
    type :: base_t
    contains
        generic :: g => impl
        procedure, private :: impl
        procedure, private :: impl2
    end type base_t
contains
    subroutine impl(self)
        class(base_t), intent(in) :: self
        hit = hit + 1
    end subroutine impl

    subroutine impl2(self)
        class(base_t), intent(in) :: self
        hit = hit + 10
    end subroutine impl2

    subroutine run(obj)
        class(base_t), intent(in) :: obj
        integer :: start
        start = hit
        call obj%impl()
        if (hit - start /= 1) error stop
        call obj%g()
        if (hit - start /= 2) error stop
        call obj%impl2()
        if (hit - start /= 12) error stop
    end subroutine run
end module type_bound_private_04_base

module type_bound_private_04_child
    use type_bound_private_04_base, only: base_t, hit
    implicit none
    private
    public :: child_t, run_child
    type, extends(base_t) :: child_t
    contains
        procedure, private :: impl => child_impl
        procedure :: impl2 => child_impl2
    end type child_t
contains
    subroutine child_impl(self)
        class(child_t), intent(in) :: self
        hit = hit + 100
    end subroutine child_impl

    subroutine child_impl2(self, n)
        class(child_t), intent(in) :: self
        integer, intent(in) :: n
        hit = hit + 1000 * n
    end subroutine child_impl2

    subroutine run_child(obj)
        class(child_t), intent(in) :: obj
        call obj%impl()
    end subroutine run_child
end module type_bound_private_04_child

program type_bound_private_04
    use type_bound_private_04_base, only: run, hit
    use type_bound_private_04_child, only: child_t, run_child
    implicit none
    type(child_t) :: c
    class(child_t), allocatable :: p
    call run(c)
    if (hit /= 12) error stop
    call run_child(c)
    if (hit /= 112) error stop
    call c%impl2(2)
    if (hit /= 2112) error stop
    allocate(p)
    call p%impl2(3)
    if (hit /= 5112) error stop
    call run(p)
    if (hit /= 5124) error stop
    call c%g()
    if (hit /= 5125) error stop
    call p%g()
    if (hit /= 5126) error stop
    print *, hit
end program type_bound_private_04
