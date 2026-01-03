program array_op_7
integer :: A(2) = [2,2]
logical :: B(2)
integer :: i
B = abs(A) > maxval(abs(A))
do i = 1, 2
    if (B(i) .neqv. .false.) error stop
end do
end program 