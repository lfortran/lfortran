! Local TARGET automatic and allocatable arrays that are also reached through
! a module pointer and through a pointer component, and written through both
! names inside the same loop.
module tbaa_11_m
implicit none
real, pointer :: gp(:) => null()
type :: view_t
    real, pointer :: p(:) => null()
end type
contains

subroutine bump_global(i)
    integer, intent(in) :: i
    gp(i) = gp(i) + 10
end subroutine

subroutine automatic(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, target :: a(n)
    type(view_t) :: v
    integer :: i
    a = 1
    gp => a
    v%p => a
    do i = 1, n
        a(i) = a(i) + i
        gp(i) = gp(i)*2
        v%p(i) = v%p(i) + a(i)
        call bump_global(i)
        a(i) = a(i) - 1
    end do
    ! a(i) = ((1 + i)*2*2 + 10) - 1
    s = sum(a)
    nullify(gp)
end subroutine

subroutine allocatable(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable, target :: a(:)
    type(view_t) :: v
    integer :: i, j
    allocate(a(n))
    a = 0
    gp => a
    v%p => a(n:1:-1)
    do j = 1, 2
        do i = 1, n
            a(i) = a(i) + 1
            gp(i) = gp(i) + a(i)
            v%p(n+1-i) = v%p(n+1-i)*3
        end do
    end do
    ! j = 1: a(i) = 6, j = 2: a(i) = (6 + 1)*2*3 = 42
    s = sum(a)
    nullify(gp)
end subroutine

end module

program tbaa_11
use tbaa_11_m
implicit none
real :: s
integer :: i, n
n = 5
call automatic(n, s)
if (abs(s - real(sum([((1 + i)*4 + 9, i = 1, n)]))) > 1e-4) error stop 1
call allocatable(n, s)
if (abs(s - 42*n) > 1e-4) error stop 2
print *, s
end program
