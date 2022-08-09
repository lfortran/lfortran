program arrays_04_func
implicit none
real :: a(3), b
a(1) = 3
a(2) = 2
a(3) = 1
b = sum(size(a, dim=1), a)
print *, b
if (abs(b-6) > 1e-5) error stop

contains

    real function sum(na1, a) result(r)
    integer, intent(in) :: na1
    real, intent(in) :: a(na1)
    integer :: i
    print *, "sum"
    r = 0
    do i = 1, size(a)
        r = r + a(i)
    end do
    end function

    real function abs(a) result(r)
    real, intent(in) :: a
    print *, "abs"
    if (a > 0) then
        r = a
    else
        r = -a
    end if
    end function

end
