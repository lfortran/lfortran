program gpu_lanes_chain_01
! A statement that adds two terms to a sum at once (`t = t + a + b`, which is
! `(t + a) + b`) updates the sum as two statements do, so the lanes of an
! iteration share the loop out and combine the sum. Every value is exact.
implicit none
integer, parameter :: rows = 5, n = 1024
real :: x(n, rows), y(n, rows), s(rows), ref
integer :: m, i
do m = 1, rows
    do i = 1, n
        x(i, m) = real(mod(i + m, 7) - 3)
        y(i, m) = real(mod(i*m, 5) - 2)
    end do
end do
s = -1
do concurrent (m = 1:rows)
    block
        real :: t
        integer :: k
        t = 0
        do k = 1, n/2
            t = t + x(2*k - 1, m)*y(2*k - 1, m) + x(2*k, m)*y(2*k, m)
        end do
        s(m) = t
    end block
end do
do m = 1, rows
    ref = 0
    do i = 1, n
        ref = ref + x(i, m)*y(i, m)
    end do
    if (s(m) /= ref) error stop "sum"
end do
print *, "ok"
end program
