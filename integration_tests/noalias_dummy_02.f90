! A TARGET or POINTER dummy argument can be modified through a pointer
! while the procedure runs (F2018 15.5.2.13), so its parameter is not
! noalias under --fast.
module noalias_dummy_02_m
implicit none
real, pointer :: gp => null()
real, pointer :: ga(:) => null()
contains

subroutine sum_target(x, n, total)
    real, target, intent(inout) :: x
    integer, intent(in) :: n
    real, intent(out) :: total
    integer :: k
    total = 0
    do k = 1, n
        gp = real(k)
        total = total + x
    end do
end subroutine

subroutine sum_target_array(x, n, total)
    real, target, intent(inout) :: x(:)
    integer, intent(in) :: n
    real, intent(out) :: total
    integer :: k
    total = 0
    do k = 1, n
        ga(2) = real(k)
        total = total + x(2)
    end do
end subroutine

subroutine sum_pointer(p, n, total)
    real, pointer, intent(in) :: p
    integer, intent(in) :: n
    real, intent(out) :: total
    integer :: k
    total = 0
    do k = 1, n
        gp = real(2*k)
        total = total + p
    end do
end subroutine

end module

program noalias_dummy_02
use noalias_dummy_02_m
implicit none
real, target :: v, w(3)
real, pointer :: q
real :: total

v = 0
gp => v
call sum_target(v, 4, total)
if (abs(total - 10) > 1e-6) error stop 1
if (abs(v - 4) > 1e-6) error stop 2

w = 0
ga => w
call sum_target_array(w, 5, total)
if (abs(total - 15) > 1e-6) error stop 3
if (abs(w(2) - 5) > 1e-6) error stop 4

q => v
call sum_pointer(q, 3, total)
if (abs(total - 12) > 1e-6) error stop 5
print *, "ok"
end program
