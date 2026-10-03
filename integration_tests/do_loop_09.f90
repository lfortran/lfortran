program do_loop_09
implicit none
integer :: i, j, k, count

! Test 1: Two nested DO loops sharing a single CONTINUE terminator
count = 0
do 10 i = 1, 3
do 10 j = 1, 4
    count = count + 1
10 continue
if (count /= 12) error stop

! Test 2: Three nested DO loops sharing a single CONTINUE terminator
count = 0
do 20 i = 1, 2
do 20 j = 1, 3
do 20 k = 1, 4
    count = count + 1
20 continue
if (count /= 24) error stop

! Test 3: Shared terminator with computation
count = 0
do 30 i = 1, 3
do 30 j = 1, 3
    if (i == j) count = count + 1
30 continue
if (count /= 3) error stop

print *, "PASS"
end program
