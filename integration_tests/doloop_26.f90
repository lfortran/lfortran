program doloop_26
implicit none
integer :: i, j, a(3)
integer :: expected(3) = [1, 2, 2]
real(8) :: u, cumulative
real(8) :: weights(2) = [0.35d0, 0.65d0]
character(len=40) :: s
do i = 1, 3
    u = real(i, 8)/4
    cumulative = 0
    do j = 1, 1
        cumulative = cumulative + weights(j)
        if (u < cumulative) exit
    end do
    print *, u, j
    if (j /= expected(i)) error stop
end do
a = [1, 2, 3]
i = 0
write (s, *) (a(i), i = 1, 2)
print *, i
if (i /= 3) error stop
end program
