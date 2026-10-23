! A derived-type variable larger than a thread keeps a copy of a part of,
! whose value is kept after a `do concurrent` (a dummy argument, and a
! local a later loop reads), that every iteration assigns whole, or assigns
! whole and then defines a part of: each thread keeps a whole copy of it,
! as large as the kernel local it would otherwise be.
module gpu_derived_component_18_m
    implicit none
    type :: b_t
        real :: c(2000)
        real :: k
    end type
contains
    subroutine run(x, y, g, g0)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        type(b_t), intent(inout) :: g
        type(b_t), intent(in) :: g0
        integer :: i
        do concurrent (i = 1:size(x))
            g = g0
            y(i) = g%k + x(i)
        end do
    end subroutine
end module

program gpu_derived_component_18
    use gpu_derived_component_18_m
    implicit none
    integer, parameter :: n = 100
    real :: x(n), b(n)
    type(b_t) :: g, g0, z
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    g0%c = 1
    g0%k = 3
    call run(x, b, g, g0)
    print *, b(7), g%k
    if (b(7) /= 10 .or. g%k /= 3) error stop 1
    do concurrent (i = 1:n)
        z = g0
        z%c(1) = x(i)
        b(i) = z%c(1) + z%k
    end do
    print *, b(7)
    if (b(7) /= 10) error stop 2
    do concurrent (i = 1:n)
        z = g0
        b(i) = z%c(2) + z%k
    end do
    print *, b(7), z%k
    if (b(7) /= 4 .or. z%k /= 3) error stop 3
end program
