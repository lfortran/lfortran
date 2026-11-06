program gpu_lanes_unroll_01
! The loops the lanes of an iteration share out step by the lanes; on Metal
! they are unrolled sixteen times all the same (gpu_sequential_unroll.cpp),
! with the iterations that do not fill a group of sixteen run after the
! groups. Rows of lengths around the group size of 16*32 elements: a sum
! accumulated by the lanes and an elementwise loop. Every value is exact.
implicit none
integer, parameter :: rows = 6
integer :: lengths(rows), m, d, i
real :: x(1100, rows), y(1100, rows), s(rows), ref
lengths = [1, 31, 512, 513, 1000, 1100]
do m = 1, rows
    do i = 1, 1100
        x(i, m) = real(mod(i + m, 9) - 4)
    end do
end do
y = -1
do concurrent (m = 1:rows)
    block
        real :: t
        integer :: k, n
        n = lengths(m)
        t = 0
        do k = 1, n
            t = t + x(k, m)*x(k, m)
        end do
        s(m) = t
        do k = 1, n
            y(k, m) = 2*x(k, m) + t
        end do
    end block
end do
do m = 1, rows
    ref = 0
    do i = 1, lengths(m)
        ref = ref + x(i, m)**2
    end do
    if (s(m) /= ref) error stop "sum"
    do i = 1, lengths(m)
        if (y(i, m) /= 2*x(i, m) + ref) error stop "elements"
    end do
    do i = lengths(m) + 1, 1100
        if (y(i, m) /= -1) error stop "beyond the row"
    end do
end do
print *, "ok"
end program
