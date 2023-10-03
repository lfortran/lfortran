program intrinsics_65

implicit none
integer :: arr_01(20) = [995, -697,  90, -514,   0, -777,  664,  -15, 401, -348, &
                         269,  -94, 534,  806, -23,  844,    0, -114, 673, -445]
integer              :: a_01(4, 3), a_02(3)
integer, allocatable :: a_03(:)
integer              :: b_01(3, 4), b_02(3), res_01(4)
real                 :: c_01(3, 5), res_02(4, 5)

allocate(a_03(3))

a_01 = reshape(arr_01, shape(a_01))
b_01 = reshape(arr_01, shape(b_01))
c_01 = reshape(real(arr_01), shape(c_01))
a_02 = a_01(1, :)
a_03 = a_01(2, :)
b_02 = b_01(:, 3)

if (sum(matmul(a_01, b_01)) /= -130358) error stop

res_01 = matmul(a_01, b_02) + 2 * matmul(a_02, b_01)
if (size(res_01) /= 4) error stop
res_02 = matmul(a_01, c_01)

if (res_01(1) /= 2873711) error stop
if (res_02(1, 1) /=  a_01(1, 1) * c_01(1, 1) &
                   + a_01(1, 2) * c_01(2, 1) &
                   + a_01(1, 3) * c_01(3, 1) ) error stop
res_01 = matmul(a_02, b_01)
if (res_01(1) /= res_02(1, 1)) error stop

end program intrinsics_65
