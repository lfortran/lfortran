program gpu_lanes_scattered_02
! A sum that reads across the columns of an array (the loop variable is the
! second subscript), followed in the same iteration by a loop that updates
! the array from the sum: the recurrence of a gated delta rule. The update
! loop is independent, which the lanes share out, but the sum is not shared
! out where the iterations fill the GPU, and the lanes would each run the
! whole sum for nothing: such a kernel keeps one lane per iteration there,
! as it does without the update loop, and has lanes only for a launch of few
! iterations. The values are small integers, so every order of the additions
! gives the same bits, which are compared exactly.
implicit none
integer, parameter :: dk = 96
real, allocatable :: st(:, :, :), ref(:, :, :), qk(:, :), y(:, :), yref(:, :)
integer :: dv, hv, row, k, h, pass
real :: a, delta
do pass = 1, 2
    if (pass == 1) then
        dv = 16; hv = 2
    else
        dv = 128; hv = 24
    end if
    allocate(st(dv, dk, hv), ref(dv, dk, hv), qk(dk, hv), y(dv, hv), yref(dv, hv))
    do h = 1, hv
        do k = 1, dk
            qk(k, h) = mod(k + 3*h, 5) - 2
            do row = 1, dv
                st(row, k, h) = mod(row + 5*k + h, 7) - 3
            end do
        end do
    end do
    ref = st
    do h = 1, hv
        do row = 1, dv
            a = 0
            do k = 1, dk
                a = a + ref(row, k, h)*qk(k, h)
            end do
            delta = mod(a, 4.0)
            yref(row, h) = a + delta
            do k = 1, dk
                ref(row, k, h) = ref(row, k, h) + qk(k, h)*delta
            end do
        end do
    end do
    do concurrent (h = 1:hv, row = 1:dv)
        block
            real :: acc, d
            integer :: j
            acc = 0
            do j = 1, dk
                acc = acc + st(row, j, h)*qk(j, h)
            end do
            d = mod(acc, 4.0)
            y(row, h) = acc + d
            do j = 1, dk
                st(row, j, h) = st(row, j, h) + qk(j, h)*d
            end do
        end block
    end do
    if (any(y /= yref)) error stop "sum"
    if (any(st /= ref)) error stop "update"
    deallocate(st, ref, qk, y, yref)
end do
print *, "ok"
end program
