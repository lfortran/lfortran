program gpu_lanes_liveout_01
! The variable of a loop that the lanes of a SIMD-group may share out
! (gpu_vector_lanes.cpp), read after the loop around it ends: the lanes
! each keep the value of their own last iteration, so such a loop runs
! whole on each lane. Each iteration of the loop around it assigns the
! variable first, which shows only that the next iteration does not read
! it, not that nothing after that loop does.
implicit none
integer, parameter :: n = 40, m = 50
integer :: last(n), i
real :: s(m + 1), total(n)
s = 1
do concurrent (i = 1:n)
    block
        integer :: d, t
        real :: acc
        acc = 0
        do d = 1, m
            do t = 1, d
                acc = s(t)
            end do
        end do
        last(i) = t
        total(i) = acc + i
    end block
end do
do i = 1, n
    if (last(i) /= m + 1) error stop "last"
    if (total(i) /= 1 + i) error stop "total"
end do
print *, "ok"
end program
