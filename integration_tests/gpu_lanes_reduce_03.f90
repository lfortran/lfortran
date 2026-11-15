program gpu_lanes_reduce_03
! Loops of an iteration that the lanes of a warp share out and combine on an
! NVIDIA GPU, whose accumulators are updated under an `if`: a maximum and a
! minimum written as `if (v > m) m = v`, a count and a sum of the elements
! that pass a test, and the chunks of a sampler (a maximum and a count of
! non-finite-like values in one loop). Every result is compared exactly with
! the loop run on the host; the values are small integers.
implicit none
integer, parameter :: n = 1003, m = 40
real :: a(n, m), mx(m), mn(m), mxr(m), mnr(m), s(m), sr(m), cnt(m), cntr(m)
integer :: i, j, c, cr
real :: t, v
do j = 1, m
    do i = 1, n
        a(i, j) = mod(7*i + 11*j, 97) - 48
    end do
end do
do j = 1, m
    t = a(1, j)
    v = a(1, j)
    cr = 0
    sr(j) = 0
    do i = 1, n
        if (a(i, j) > t) t = a(i, j)
        if (a(i, j) < v) v = a(i, j)
        if (a(i, j) < -40) cr = cr + 1
        if (mod(i, 3) == 0) sr(j) = sr(j) + a(i, j)
    end do
    mxr(j) = t
    mnr(j) = v
    cntr(j) = cr
end do
do concurrent (j = 1:m)
    block
        real :: hi, lo, total
        integer :: k, count
        hi = a(1, j)
        lo = a(1, j)
        count = 0
        total = 0
        do k = 1, n
            if (a(k, j) > hi) hi = a(k, j)
            if (lo > a(k, j)) lo = a(k, j)
            if (a(k, j) < -40) count = count + 1
            if (mod(k, 3) == 0) then
                total = total + a(k, j)
            end if
        end do
        mx(j) = hi
        mn(j) = lo
        cnt(j) = count
        s(j) = total
    end block
end do
if (any(mx /= mxr)) error stop "max"
if (any(mn /= mnr)) error stop "min"
if (any(cnt /= cntr)) error stop "count"
if (any(s /= sr)) error stop "sum"
print *, "ok"
end program
