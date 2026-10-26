! A local derived-type variable too large for a thread to copy, or of a
! type with an allocatable component, not read after a `do concurrent`, of
! which every iteration defines an element of an array component it owns
! or a component of a component, and reads a component the loop does not
! define: the variable stays a kernel argument, so that component keeps
! the value it had before the loop.
module gpu_derived_component_16_m
    implicit none
    type :: big_t
        real :: c(2000)
        integer :: k
    end type
    type :: alloc_t
        real :: c(1000)
        real, allocatable :: a(:)
        integer :: k
    end type
    type :: in_t
        integer :: k
    end type
    type :: nest_t
        real :: c(2000)
        integer :: k
        type(in_t) :: in
    end type
end module

program gpu_derived_component_16
    use gpu_derived_component_16_m
    implicit none
    integer, parameter :: n = 1000
    type(big_t) :: y
    type(alloc_t) :: z
    type(nest_t) :: w
    real :: x(n), b(n)
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    y%k = 5
    do concurrent (i = 1:n)
        y%c(i) = x(i)
        b(i) = y%c(i) + y%k
    end do
    print *, b(7), b(n)
    if (b(7) /= 12 .or. b(n) /= n + 5) error stop 1
    allocate(z%a(3))
    z%k = 7
    do concurrent (i = 1:n)
        z%c(i) = x(i)
        b(i) = z%c(i) + z%k
    end do
    print *, b(7), b(n)
    if (b(7) /= 14 .or. b(n) /= n + 7) error stop 2
    w%k = 3
    w%c = 3
    w%in%k = 0
    do concurrent (i = 1:n)
        w%in%k = i
        b(i) = x(i) + w%k + w%in%k + w%c(i)
    end do
    print *, b(7), b(n)
    if (b(7) /= 20 .or. b(n) /= 2 * n + 6) error stop 3
end program
