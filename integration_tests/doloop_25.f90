program doloop_25
implicit none
integer :: i, a, b, c, count
integer(8) :: i8, a8, b8
a = -huge(a) - 1
b = a + 3
c = 2
count = 0
do i = a, b
    count = count + 1
end do
if (count /= 4 .or. i /= a + 4) error stop
count = 0
do i = a, b, c
    count = count + 1
end do
if (count /= 2 .or. i /= a + 4) error stop
count = 0
do i = a, b
    count = count + 1
    exit
end do
if (count /= 1 .or. i /= a) error stop
count = 0
do i = a, b
    if (i == a) cycle
    count = count + 1
    exit
end do
if (count /= 1 .or. i /= a + 1) error stop
a = huge(a)
b = a - 3
count = 0
do i = a, b, -1
    count = count + 1
end do
if (count /= 4 .or. i /= a - 4) error stop
count = 0
do i = a, b, -1
    count = count + 1
    exit
end do
if (count /= 1 .or. i /= a) error stop
a8 = -huge(a8) - 1_8
b8 = a8 + 3_8
count = 0
do i8 = a8, b8
    count = count + 1
end do
if (count /= 4 .or. i8 /= a8 + 4_8) error stop
count = 0
do i8 = a8, b8
    count = count + 1
    exit
end do
if (count /= 1 .or. i8 /= a8) error stop
a8 = huge(a8)
b8 = a8 - 3_8
count = 0
do i8 = a8, b8, -1_8
    count = count + 1
end do
if (count /= 4 .or. i8 /= a8 - 4_8) error stop
count = 0
do i8 = a8, b8, -1_8
    count = count + 1
    exit
end do
if (count /= 1 .or. i8 /= a8) error stop
end program
