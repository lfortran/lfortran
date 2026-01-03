program intrinsics_198
real, dimension(2,3) :: x
real :: res
x = 12.9
res = var_2_rsp_rsp(x, 1)
print *, res
if (abs(res - 77.3999939) > 1e-8) error stop
contains
function var_2_rsp_rsp(x, dim) result(res)
    real, intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real :: res
    res = sum(sum(x, dim))
end function var_2_rsp_rsp
end program
