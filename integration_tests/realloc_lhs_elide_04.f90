! Which intrinsic assignments to allocatable arrays are reallocated, which
! allocate without a check, and which need neither (the reference test of
! this file shows them after the array_op pass).
module realloc_lhs_elide_04_mod
    implicit none
contains
    subroutine step(x, v, dt)
        real, allocatable, intent(inout) :: x(:), v(:)
        real, intent(in) :: dt
        real, allocatable :: t(:), u(:)
        ! allocate: t is unallocated
        t = v
        ! neither: x and v are whole operands of the values
        x = x + dt*v
        v = abs(v)
        ! neither: the assignment to t above gave it the shape of v
        v = t
        ! reallocate: x is referenced, but not as a whole operand
        x = x(2:)
        ! allocate, then neither
        u = x
        u = 2*x
        ! reallocate: u is not known to have the shape of v
        u = v
        ! allocate: t has just been deallocated
        deallocate(t)
        t = u + 1
        call check(u, t)
    end subroutine

    subroutine check(u, t)
        real, intent(in) :: u(:), t(:)
        if (size(u) /= 3 .or. any(u /= [4., 5., 6.])) error stop 1
        if (size(t) /= 3 .or. any(t /= [5., 6., 7.])) error stop 2
    end subroutine
end module

program realloc_lhs_elide_04
    use realloc_lhs_elide_04_mod
    implicit none
    real, allocatable :: x(:), v(:)
    x = [1., 2., 3.]
    v = [4., 5., 6.]
    call step(x, v, 0.5)
    if (size(x) /= 2 .or. any(x /= [4.5, 6.])) error stop 3
    if (size(v) /= 3 .or. any(v /= [4., 5., 6.])) error stop 4
    print *, "ok"
end program
