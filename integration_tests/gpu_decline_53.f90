! gpu decline reason: ArrayReductionElement, ArrayReductionType,
! ArrayReductionLoop, ArrayReductionDevice
! A REDUCE of an array is combined on the device only where every iteration
! names the same elements of it (gpu_offload_array_reduction.h): an element
! an iteration chooses, as in a histogram, would need atomic updates, and so
! would the whole array. Nor is it combined for a type or an operation the
! device does not combine, or in a loop that does not run its iterations in
! a grid-stride loop (one that cycles, or that defines a scalar used after
! it). The last loop is combined on CUDA; Metal combines none of them.
program gpu_decline_53
    implicit none
    integer :: i, idx(100), h(5), m(2)
    real :: x(100), s(3), t
    logical :: flags(2)

    do i = 1, 100
        idx(i) = mod(i, 5) + 1
        x(i) = real(mod(i, 7) - 3)
    end do

    h = 0
    do concurrent (i = 1:100) reduce(+: h)
        h(idx(i)) = h(idx(i)) + 1
    end do
    if (sum(h) /= 100) error stop 1

    s = 0
    do concurrent (i = 1:100) reduce(+: s)
        s = s + x(i)
    end do
    if (s(2) /= sum(x)) error stop 2

    flags = .false.
    do concurrent (i = 1:100) reduce(.or.: flags)
        flags(1) = flags(1) .or. x(i) > 2
    end do
    if (.not. flags(1) .or. flags(2)) error stop 3

    m = -1
    do concurrent (i = 1:100) reduce(iand: m)
        m(2) = iand(m(2), i)
    end do
    if (m(2) /= 0 .or. m(1) /= -1) error stop 4

    s = 0
    do concurrent (i = 1:100) reduce(+: s)
        if (x(i) < 0) cycle
        s(1) = s(1) + x(i)
    end do
    if (s(1) /= sum(x, x >= 0)) error stop 5

    t = 0
    s = 0
    do concurrent (i = 1:100) reduce(+: s)
        if (i == 100) t = x(i)
        s(2) = s(2) + x(i)
    end do
    if (t /= x(100) .or. s(2) /= sum(x)) error stop 6

    s = 0
    do concurrent (i = 1:100) reduce(+: s)
        s(3) = s(3) + x(i)
    end do
    if (s(3) /= sum(x) .or. s(1) /= 0) error stop 7
    t = s(3)
    print *, t
end program
