program gpu_metal_526
! Index-set splitting must not use the value of a scalar assigned in a
! branch after the branch or in another branch: with LOCAL_INIT the
! scalar keeps its value from outside where the branch does not run.
! The checks hold whatever value the scalar has there: `m == i + 5` and
! `k == i + 50` hold for at most one other iteration, while a split
! that kept the bound value made them hold for the whole interior.
implicit none
integer :: n, i, m, k
integer :: b(40), c(40)
n = 40
m = 0
b = -1
do concurrent (i = 1:n) local_init(m)
    if (i == 1) m = i + 5
    if (i == n) then
        b(i) = 0
    else if (m == i + 5) then
        b(i) = 1
    else
        b(i) = 2
    end if
end do
print *, b(1), b(2), b(20), b(40)
if (b(1) /= 1 .or. b(40) /= 0) error stop 1
if (any(b(2:39) /= 1 .and. b(2:39) /= 2)) error stop 1
if (count(b(2:39) == 1) > 1) error stop 1

k = 0
c = -1
do concurrent (i = 1:n) local_init(k)
    select case (i)
    case (1)
        k = i + 50
    case default
        if (k == i + 50) then
            c(i) = 5
        else
            c(i) = 6
        end if
    end select
    if (i == 1) then
        c(i) = 7
    else if (i < n .and. k == i + 50) then
        c(i) = c(i) + 10
    end if
end do
print *, c(1), c(2), c(20), c(40)
if (c(1) /= 7 .or. (c(40) /= 5 .and. c(40) /= 6)) error stop 2
if (any(c(2:39) /= 6 .and. c(2:39) /= 15)) error stop 2
if (count(c(2:39) == 15) > 1) error stop 2
print *, "ok"
end program
