program gpu_metal_731
! An array each iteration of a do concurrent loop defines whole by an
! assignment to a section over its full extent, before any reference, is
! private to the iteration (lfortran/lfortran#17013). Read at a run-time
! index, one array all GPU threads shared gave other iterations' values.
implicit none
integer, parameter :: n = 10000
integer :: tmp(4), b(3, 2), out(n), i

do concurrent (i = 1:n)
    tmp(:) = i
    if (mod(i, 7) == 0) tmp(3) = 0
    out(i) = tmp(mod(i, 4) + 1)
end do
do i = 1, n
    if (mod(i, 7) == 0 .and. mod(i, 4) == 2) then
        if (out(i) /= 0) error stop 1
    else
        if (out(i) /= i) error stop 2
    end if
end do

do concurrent (i = 1:n)
    tmp(1:4) = 2 * i
    out(i) = tmp(mod(i, 4) + 1)
end do
do i = 1, n
    if (out(i) /= 2 * i) error stop 3
end do

do concurrent (i = 1:n)
    tmp(lbound(tmp, 1):ubound(tmp, 1)) = 3 * i
    out(i) = tmp(mod(i, 4) + 1)
end do
do i = 1, n
    if (out(i) /= 3 * i) error stop 4
end do

do concurrent (i = 1:n)
    tmp(:4) = 4 * i
    out(i) = tmp(mod(i, 4) + 1)
end do
do i = 1, n
    if (out(i) /= 4 * i) error stop 5
end do

do concurrent (i = 1:n)
    b(:, :) = i
    b(mod(i, 3) + 1, 2) = -i
    out(i) = b(mod(i, 3) + 1, mod(i, 2) + 1) + b(mod(i + 1, 3) + 1, 2)
end do
do i = 1, n
    if (mod(i, 2) == 0) then
        if (out(i) /= 2 * i) error stop 6
    else
        if (out(i) /= 0) error stop 7
    end if
end do

do concurrent (i = 1:n)
    b(1:3, 1:2) = 5 * i
    out(i) = b(mod(i, 3) + 1, mod(i, 2) + 1)
end do
do i = 1, n
    if (out(i) /= 5 * i) error stop 8
end do
! The bounds of a section an inquiry is about are evaluated: this reads
! tmp(1), so tmp is not defined before it is referenced.
tmp = 1
do concurrent (i = 1:1)
    tmp = ubound(tmp(tmp(1):4), 1)
    out(i) = tmp(4)
end do
if (out(1) /= 4) error stop 9
print *, "ok"
end program
