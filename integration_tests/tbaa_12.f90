! An automatic array passed to a procedure that saves its address in a
! global, then written through the saved pointer and directly in a loop.
module tbaa_12_m
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
implicit none
real, pointer :: saved(:) => null()
type(c_ptr) :: saved_c
contains

subroutine remember(x)
    real, target, intent(inout) :: x(:)
    saved => x
    saved_c = c_loc(x)
end subroutine

subroutine through_saved(i, v)
    integer, intent(in) :: i
    real, intent(in) :: v
    real, pointer :: q(:)
    saved(i) = saved(i) + v
    call c_f_pointer(saved_c, q, [size(saved)])
    q(i) = q(i)*2
end subroutine

subroutine work(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, target :: a(n)
    real :: b(n)
    integer :: i
    a = 1
    b = 0
    call remember(a)
    do i = 1, n
        a(i) = a(i) + i
        call through_saved(i, 3.0)
        b(i) = a(i)
        saved(i) = saved(i) - 1
        a(i) = a(i) + b(i)
    end do
    ! the call makes a(i) = (1 + i + 3)*2, then a(i) = 2*a(i) - 1
    s = sum(a)
    nullify(saved)
end subroutine

subroutine fixed(s)
    real, intent(out) :: s
    real, target :: a(6)
    integer :: i
    a = 2
    call remember(a)
    do i = 1, 6
        a(i) = a(i) + i
        call through_saved(i, 1.0)
        a(i) = a(i) + 1
    end do
    ! a(i) = (2 + i + 1)*2 + 1
    s = sum(a)
    nullify(saved)
end subroutine

end module

program tbaa_12
use tbaa_12_m
implicit none
real :: s
integer :: i, n
n = 7
call work(n, s)
if (abs(s - real(sum([((4 + i)*4 - 1, i = 1, n)]))) > 1e-4) error stop 1
call fixed(s)
if (abs(s - real(sum([((3 + i)*2 + 1, i = 1, 6)]))) > 1e-4) error stop 2
print *, s
end program
