! Allocation on assignment of an allocatable array that is unallocated
! before it: a local array before anything uses it, an array just
! deallocated, an intent(out) dummy, and arrays that something before the
! assignment can allocate, which must still be checked and reallocated.
module realloc_lhs_elide_03_mod
    implicit none
    real, allocatable :: shared(:)
contains
    subroutine first_use(v, n)
        real, intent(in) :: v(:)
        integer, intent(in) :: n
        real, allocatable :: t(:), w(:)
        integer :: m
        m = size(v)
        t = 2*v
        w = t(1:n)
        if (size(t) /= m .or. lbound(t, 1) /= 1) error stop 1
        if (any(t /= 2*v)) error stop 2
        if (size(w) /= n) error stop 3
    end subroutine

    subroutine keeps_bounds(v)
        real, allocatable, intent(in) :: v(:)
        real, allocatable :: t(:)
        t = v
        if (lbound(t, 1) /= lbound(v, 1) .or. ubound(t, 1) /= ubound(v, 1)) error stop 4
        deallocate(t)
        t = v + 1
        if (lbound(t, 1) /= 1 .or. size(t) /= size(v)) error stop 5
        if (allocated(t)) deallocate(t)
        t = v
        if (lbound(t, 1) /= lbound(v, 1)) error stop 6
    end subroutine

    subroutine out_dummy(x, v)
        real, allocatable, intent(out) :: x(:)
        real, intent(in) :: v(:)
        x = v
    end subroutine

    subroutine saved(v)
        real, intent(in) :: v(:)
        real, allocatable, save :: t(:)
        t = v
        if (size(t) /= size(v) .or. any(t /= v)) error stop 7
    end subroutine

    subroutine allocate_it(a)
        real, allocatable, intent(out) :: a(:)
        allocate(a(7))
        a = 1
    end subroutine

    subroutine passed_before(v)
        real, intent(in) :: v(:)
        real, allocatable :: t(:)
        call allocate_it(t)
        t = v
        if (size(t) /= size(v)) error stop 8
    end subroutine

    subroutine host_allocates(v)
        real, intent(in) :: v(:)
        real, allocatable :: t(:)
        call inner()
        t = v
        if (size(t) /= size(v)) error stop 9
    contains
        subroutine inner()
            allocate(t(11))
        end subroutine
    end subroutine

    subroutine assigned_in_loop(v)
        real, intent(in) :: v(:)
        real, allocatable :: t(:)
        integer :: i
        do i = 1, 2
            t = v(1:i)
        end do
        t = v
        if (size(t) /= size(v)) error stop 10
    end subroutine

    subroutine assigned_in_branch(v, flag)
        real, intent(in) :: v(:)
        logical, intent(in) :: flag
        real, allocatable :: t(:)
        if (flag) t = v(1:1)
        t = v
        if (size(t) /= size(v)) error stop 11
    end subroutine

    function make(v) result(r)
        real, intent(in) :: v(:)
        real, allocatable :: r(:)
        r = v + 1
    end function

    subroutine allocates_shared()
        shared = [1., 2., 3., 4.]
    end subroutine
end module

program realloc_lhs_elide_03
    use realloc_lhs_elide_03_mod
    implicit none
    real, allocatable :: x(:), y(:), z(:)
    integer :: i

    ! A local array of the program before anything uses it
    y = [1., 2., 3.]
    if (size(y) /= 3) error stop 20

    do i = 1, 3
        call first_use([1., 2., 3., 4., 5.], i)
        call first_use([1., 2.], 1)
    end do

    allocate(z(-1:2))
    z = 5
    call keeps_bounds(z)

    x = [1., 2., 3., 4., 5., 6.]
    call out_dummy(x, [7., 8.])
    if (size(x) /= 2 .or. any(x /= [7., 8.])) error stop 21
    call out_dummy(x, [1., 2., 3.])
    if (size(x) /= 3 .or. any(x /= [1., 2., 3.])) error stop 22

    call saved([1., 2.])
    call saved([1., 2., 3.])
    call passed_before([1., 2.])
    call host_allocates([1., 2.])
    call assigned_in_loop([1., 2., 3.])
    call assigned_in_branch([1., 2., 3.], .true.)
    call assigned_in_branch([1., 2., 3.], .false.)

    do i = 1, 2
        x = make([real(i), 2.])
        if (size(x) /= 2 .or. x(1) /= i + 1) error stop 23
    end do

    ! Allocated by a procedure before the assignment
    call allocates_shared()
    shared = y
    if (size(shared) /= 3) error stop 24

    print *, "ok"
end program
