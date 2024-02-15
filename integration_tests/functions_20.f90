pure function diag_rsp_mat(A, k) result(res)
real, intent(in) :: A(:,:)
integer, intent(in) :: k
real :: res(minval(shape(A)) - abs(k))

res = 123.71_4
end function diag_rsp_mat

program functions_20
real, dimension(3,3) :: A
real :: B(2)
integer :: k

interface
    function diag_rsp_mat(A, k) result(res)
        real, intent(in) :: A(:,:)
        integer, intent(in) :: k
        real :: res(minval(shape(A)) - abs(k))
    end function diag_rsp_mat
end interface

k = 1
B = diag_rsp_mat(A, k)
print *, B
if (any(abs(B - 123.71_4) > 1e-6)) error stop
end program functions_20
