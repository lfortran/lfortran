! A local derived-type variable, not read after a `do concurrent`, that
! every iteration first assigns whole and then defines a part of: an
! element of an allocatable component, an element of a large array
! component, and a component of a component. Each thread keeps it as a
! kernel local of its own, which the whole assignment defines.
module gpu_derived_component_17_m
    implicit none
    type :: a_t
        real, allocatable :: w(:)
        real :: k
    end type
    type :: in_t
        integer :: k
    end type
    type :: b_t
        real :: c(2000)
        real :: k
        type(in_t) :: in
    end type
end module

program gpu_derived_component_17
    use gpu_derived_component_17_m
    implicit none
    integer, parameter :: n = 3
    real :: x(n), b(n)
    integer :: i
    type(a_t) :: y, y0
    type(b_t) :: z, v, z0
    do i = 1, n
        x(i) = 10 * i
    end do
    allocate(y0%w(n))
    y0%w = 0
    y0%k = 2
    do concurrent (i = 1:n)
        y = y0
        y%w(i) = x(i)
        b(i) = y%w(i) + y%k
    end do
    print *, b
    if (any(b /= [12, 22, 32])) error stop 1
    z0%c = 1
    z0%k = 3
    z0%in%k = 0
    do concurrent (i = 1:n)
        z = z0
        z%c(1) = x(i)
        b(i) = z%c(1) + z%k
    end do
    print *, b
    if (any(b /= [13, 23, 33])) error stop 2
    do concurrent (i = 1:n)
        v = z0
        v%in%k = i
        b(i) = v%c(2) + v%k + v%in%k
    end do
    print *, b
    if (any(b /= [5, 6, 7])) error stop 3
end program
