! A type-bound generic of the parent keeps its PRIVATE specific when an
! extension declared in another module has a new binding of the same name
! (F2023 7.5.7.3). The kept specific, a binding of the parent's module, is
! still matched by its NOPASS or PASS(self) attribute in the extension.
! GFortran treats the new binding as an override; Flang and ifx follow the
! standard.
module type_bound_private_06_base
    implicit none
    private
    public :: base_t, hit
    integer :: hit = 0
    type :: base_t
    contains
        generic :: g => impl
        generic :: h => impl2
        procedure, private, nopass :: impl
        procedure, private, pass(self) :: impl2
    end type base_t
contains
    subroutine impl(n)
        integer, intent(in) :: n
        hit = hit + n
    end subroutine impl

    subroutine impl2(n, self)
        integer, intent(in) :: n
        class(base_t), intent(in) :: self
        hit = hit + 10 * n
    end subroutine impl2
end module type_bound_private_06_base

module type_bound_private_06_child
    use type_bound_private_06_base, only: base_t, hit
    implicit none
    private
    public :: child_t
    type, extends(base_t) :: child_t
    contains
        procedure, nopass :: impl => child_impl
        procedure :: impl2 => child_impl2
    end type child_t
contains
    subroutine child_impl(n)
        integer, intent(in) :: n
        hit = hit + 100 * n
    end subroutine child_impl

    subroutine child_impl2(self)
        class(child_t), intent(in) :: self
        hit = hit + 1000
    end subroutine child_impl2
end module type_bound_private_06_child

program type_bound_private_06
    use type_bound_private_06_base, only: hit
    use type_bound_private_06_child, only: child_t
    implicit none
    type(child_t) :: c
    class(child_t), allocatable :: p
    call c%g(1)
    if (hit /= 1) error stop
    allocate(p)
    call p%g(2)
    if (hit /= 3) error stop
    call c%h(3)
    if (hit /= 33) error stop
    call c%impl(1)
    if (hit /= 133) error stop
    call c%impl2()
    if (hit /= 1133) error stop
    print *, hit
end program type_bound_private_06
