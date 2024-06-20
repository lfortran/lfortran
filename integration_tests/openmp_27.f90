program openmp_27
use omp_lib
implicit none
real ( kind = 8 ) factor
integer ( kind = 4 ) i
integer ( kind = 4 ) n
real ( kind = 8 ) wtime
real ( kind = 8 ), allocatable, dimension ( : ) :: x 
real ( kind = 8 ) xdoty 
real ( kind = 8 ), allocatable, dimension ( : ) :: y

n = 100

do while ( n < 1000000 )

n = n * 10

allocate ( x(1:n) )
allocate ( y(1:n) )

factor = real ( n, kind = 8 )
factor = 1.0D+00 / sqrt ( 2.0D+00 * factor * factor + 3 * factor + 1.0D+00 )

do i = 1, n
x(i) = i * factor
end do

do i = 1, n
y(i) = i * 6 * factor
end do

call test02( n, x, y, xdoty )

deallocate ( x )
deallocate ( y )

end do
end
subroutine test02 ( n, x, y, xdoty )
use omp_lib
implicit none

integer n

integer ( kind = 4 ) i
real ( kind = 8 ) xdoty
real ( kind = 8 ) x(n)
real ( kind = 8 ) y(n)
real ( kind = 8 ) wtime

xdoty = 0.0D+00

wtime = omp_get_wtime()

!$omp parallel shared(x, y) private(i) reduction(+:xdoty)
!$omp do
do i = 1, n
xdoty = xdoty + x(i) * y(i)
end do
!$omp end do
!$omp end parallel

wtime = omp_get_wtime() - wtime

print *, 'Parallel  ', n, xdoty, wtime
if (abs(xdoty - n) > 1e-8) error stop

return
end
