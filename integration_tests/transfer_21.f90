program transfer_21
implicit none
integer(1) :: arr(4)
integer(4) :: res(1)
arr = [1_1, 2_1, 3_1, 4_1]
res = transfer(arr(1:4), [0_4])
print *, res(1)
if (res(1) /= 67305985) error stop
end program
