! A scalar component of a derived-type variable that every iteration of a
! `do concurrent` defines and then reads is a temporary of the iteration
! (Fortran 2018, 11.1.7.5): each iteration reads its own value, whether
! the variable is a dummy argument or a local the host reads after the
! loop, while the one iteration that defines another component leaves its
! value behind and the writes to an allocatable array component reach it.
module gpu_derived_component_03_m
    implicit none
    type :: u
        real, allocatable :: w(:)
        integer :: k
        integer :: last
    end type
contains
    subroutine s(self, a)
        type(u), intent(inout) :: self
        integer, intent(out) :: a(:)
        integer :: i
        do concurrent (i = 1:size(a))
            self%k = i
            a(i) = self%k
            self%w(i) = self%w(i) + 1
            if (i == size(a)) self%last = 2 * i
        end do
    end subroutine
end module

program gpu_derived_component_03
    use gpu_derived_component_03_m
    implicit none
    integer, parameter :: n = 100000
    type(u) :: y, z
    integer, allocatable :: a(:), b(:)
    integer :: i
    allocate(a(n), b(n), y%w(n))
    y%w = 1
    y%k = 0
    y%last = 0
    call s(y, a)
    do i = 1, n
        if (a(i) /= i) error stop 1
    end do
    if (any(y%w /= 2)) error stop 2
    if (y%last /= 2 * n) error stop 3
    z%k = 0
    z%last = 0
    do concurrent (i = 1:n)
        z%k = 3 * i
        b(i) = z%k - 2 * i
        if (i == 7) z%last = i
    end do
    do i = 1, n
        if (b(i) /= i) error stop 4
    end do
    if (z%last /= 7) error stop 5
    print *, a(1), a(n), b(1), b(n), y%last, z%last
end program
