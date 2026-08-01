program critical_01
implicit none
integer :: counter
integer :: i

counter = 0
do i = 1, 5
    critical
        counter = counter + 1
    end critical
end do
if (counter /= 5) error stop

sync all
if (num_images() /= 1) error stop

print *, "ok"
end program critical_01
