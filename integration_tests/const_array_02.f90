program const_array_02
implicit none
integer, parameter :: dp = kind(0.d0)
real(dp), parameter :: A(5) = [1.1_dp, 3.0_dp, 10.0_dp, 2.1_dp, 5.5_dp]
integer, parameter :: B(5) = [1, 3, 10, 2, 5]
print *, A
print *, B
end
