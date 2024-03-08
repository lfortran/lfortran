program functions_23
real :: x(5, 5)
real :: res(5)
x = 4.59
res = corr_2_rsp_rsp(x, 1)
print *, res
if (any(abs(res - 21.0681019) > 1e-6)) error stop
contains
function corr_2_rsp_rsp(x, dim, mask) result(res)
real, intent(in) :: x(:, :)
integer, intent(in) :: dim
logical, intent(in), optional :: mask
real :: res(merge(size(x, 1), size(x, 2), mask = 1<dim))

res = x(1, 1) * x(1, 1)
end function corr_2_rsp_rsp
end program
