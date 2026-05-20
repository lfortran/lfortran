module func_call_in_decl_01_mod
implicit none
contains

function f1(xx) result(tf)
    real, intent(in) :: xx(:)
    logical :: tf(size(xx))
    tf = .true.
end function f1

function f2(xx, good) result(tf)
    real, intent(in) :: xx(:)
    logical, intent(in) :: good(:)
    logical :: tf(size(xx))
    integer :: iuse(count(good))
    integer :: i, j
    j = 0
    do i = 1, size(good)
        if (good(i)) then
            j = j + 1
            iuse(j) = i
        end if
    end do
    tf = .false.
    tf(iuse) = f1(xx(iuse))
end function f2

end module func_call_in_decl_01_mod

program func_call_in_decl_01
use func_call_in_decl_01_mod
implicit none
real :: x(5)
logical :: mask(5), res(5)
x = [1.0, 2.0, 3.0, 4.0, 5.0]
mask = [.true., .false., .true., .false., .true.]
res = f2(x, mask)
if (res(1) .neqv. .true.) error stop
if (res(2) .neqv. .false.) error stop
if (res(3) .neqv. .true.) error stop
if (res(4) .neqv. .false.) error stop
if (res(5) .neqv. .true.) error stop
print *, "PASS"
end program func_call_in_decl_01
