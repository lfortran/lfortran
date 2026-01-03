subroutine strsm( a, n )
integer :: i, n
real :: a(0 : *), res
print *,"x"
res = 0.0
do i = 0, n - 1
res = res + a(i)
end do
print *, res
end
