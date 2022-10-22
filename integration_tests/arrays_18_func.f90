program arrays_18_func

real :: y(20)
real :: a(20)
integer :: i
call f(a, 20, y, 1)
do i = 1, 20
    if (2 * y(i) /= a(i)) error stop
end do

    contains

recursive subroutine f(a, n, x, i)
    integer :: n, i
    real :: a(:)
    real :: x(n)
    x(i) = i
    a(i) = 2 * i
    if (i < n) then
        call f(a, n, x, i + 1)
    end if
end subroutine

end program
