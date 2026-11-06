program derived_types_254
    ! A pointer array component reached through another component,
    ! a%b%p, passed as an actual argument to an explicit-shape dummy
    implicit none
    type :: inner_t
        integer, pointer :: p(:) => null()
    end type
    type :: outer_t
        type(inner_t) :: b
    end type
    type :: outer_ptr_t
        type(inner_t), pointer :: b => null()
    end type
    type(outer_t) :: a
    type(outer_ptr_t) :: c
    integer :: s

    allocate(a%b%p(4))
    a%b%p = [1, 2, 3, 4]
    call sumit(4, a%b%p, s)
    if (s /= 10) error stop
    call scale(4, a%b%p)
    if (any(a%b%p /= [2, 4, 6, 8])) error stop

    allocate(c%b)
    allocate(c%b%p(3))
    c%b%p = [5, 6, 7]
    call sumit(3, c%b%p, s)
    if (s /= 18) error stop
    call scale(3, c%b%p)
    if (any(c%b%p /= [10, 12, 14])) error stop

    deallocate(a%b%p, c%b%p)
    deallocate(c%b)
    print *, "ok"
contains
    subroutine sumit(n, x, s)
        integer, intent(in) :: n, x(n)
        integer, intent(out) :: s
        s = sum(x)
    end subroutine
    subroutine scale(n, x)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        x = 2*x
    end subroutine
end program
