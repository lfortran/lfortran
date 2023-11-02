program array_04_all
implicit none
integer :: i, j
logical l1(4)
logical, dimension(2, 3) :: l2

l1 = [.true., .true., .true., .true.]
if (.not. all(l1)) error stop
print *, l1

l2 = reshape([.true., .true., .true., .true., .false., .true.], [2, 3])
if (.not. all(l2(:, 1))) error stop
do i = 1, 2
    do j = 1, 3
        print *, l2(i, j)
    end do
end do

end program
