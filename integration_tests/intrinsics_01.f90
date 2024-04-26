program intrinsics_01
use lcompilers_test_module, only: lcompilers_test
integer, parameter :: dp = kind(0.d0)
real(dp) :: a, b, c(4)
integer :: i
a = 1.1_dp
b = 1.2_dp
call lcompilers_test(b-a, 0.1_dp)
call lcompilers_test(abs(b-a), 0.1_dp)
call lcompilers_test(abs(a-b), 0.1_dp)
a = 4._dp
call lcompilers_test(sqrt(a), 2._dp)

a = 4._dp
call lcompilers_test(log(a), 1.3862943611198906_dp)

c(1) = -1._dp
c(2) = -1._dp
c(3) = -1._dp
c(4) = -1._dp
call random_number(c)
do i = 1, 4
    call lcompilers_test(c(i) > 0._dp)
    call lcompilers_test(c(i) < 1._dp)
end do
end
