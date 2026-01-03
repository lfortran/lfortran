program parameter_11
integer, parameter :: n = 3
real:: v(n), a(n,n), b(n,n)
integer :: i,j

v = [(i,i=1,n)]
a = diag_rsp(v)
b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
print *, b
print *, "all(a==b)", all(a==b)
if (.not. all(a==b)) error stop
contains
function diag_rsp(v) result(res)
    real, intent(in) :: v(:)
    real :: res(size(v),size(v))
    integer :: i
    res = 0
    do i = 1, size(v)
      res(i,i) = v(i)
    end do
end function diag_rsp
end program
