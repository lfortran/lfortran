program intrinsics_250
integer, dimension(2,2) :: x
real(8) :: res(2)
x = 2
res = func(x, 1)
contains

function func(x, dim) result(res)
integer, intent(in) :: x(:,:)
integer, intent(in) :: dim
real(8) :: res(2)
print*, sum(abs(x), dim)
res = sum(abs(x), dim)
if (any(res /= [4, 4])) error stop
end function func
end program   