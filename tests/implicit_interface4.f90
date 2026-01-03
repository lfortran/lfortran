subroutine driver(f, n)
integer, intent(in) :: n
real :: i
real(8) :: j
call f(n, i, j)
call driver2(f)
end subroutine

