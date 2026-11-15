module gpu_lanes_concurrent_01_m
! The decode step of attention for a few heads, in one kernel: each head is
! an iteration of the outer `do concurrent`, and the phases of the step are
! `do concurrent` loops inside it. A nested `do concurrent` says that its own
! iterations are the ones to share out between the lanes of the head, so the
! scores of the positions are taken one position per lane, each with the
! plain loop over the head dimension that its lane sums (not one position at
! a time with the lanes sharing that short sum), and the lanes wait for each
! other between the phases (gpu_vector_lanes.cpp).
implicit none
contains
    pure subroutine attention(q, k, v, out, scores, group, length)
        real, contiguous, intent(in) :: q(:, :), k(:, :, :), v(:, :, :)
        real, contiguous, intent(out) :: out(:, :)
        real, contiguous, intent(inout) :: scores(:, :)
        integer, intent(in) :: group, length
        integer :: h
        do concurrent (h = 1:size(q, 2))
            block
                integer :: kh, t, d, u
                real :: s, top, total, acc
                kh = (h - 1)/group + 1
                do concurrent (t = 1:length)
                    s = 0
                    do d = 1, size(q, 1)
                        s = s + q(d, h)*k(d, kh, t)
                    end do
                    scores(t, h) = s
                end do
                top = -huge(top)
                do concurrent (t = 1:length) reduce(max:top)
                    top = max(top, scores(t, h))
                end do
                do concurrent (t = 1:length)
                    scores(t, h) = exp(scores(t, h) - top)
                end do
                total = 0
                do concurrent (t = 1:length) reduce(+:total)
                    total = total + scores(t, h)
                end do
                do concurrent (d = 1:size(q, 1))
                    acc = 0
                    do u = 1, length
                        acc = acc + scores(u, h)*v(d, kh, u)
                    end do
                    out(d, h) = acc/total
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_concurrent_01
use gpu_lanes_concurrent_01_m
implicit none
integer, parameter :: dh = 64, nh = 6, nkv = 2, length = 150
real :: q(dh, nh), k(dh, nkv, length), v(dh, nkv, length), out(dh, nh)
real :: scores(length, nh), e(length), top, total, want
integer :: h, kh, t, d, n, used
do h = 1, nh
    do d = 1, dh
        q(d, h) = real(mod(3*d + 5*h, 7) - 3)/8
    end do
end do
do t = 1, length
    do kh = 1, nkv
        do d = 1, dh
            k(d, kh, t) = real(mod(d + 2*kh + 3*t, 11) - 5)/16
            v(d, kh, t) = real(mod(2*d + kh + t, 13) - 6)/4
        end do
    end do
end do
do n = 1, 2
    ! Fewer iterations than a warp, where the lanes share the sums instead.
    used = merge(length, 20, n == 1)
    call attention(q, k, v, out, scores, nh/nkv, used)
    do h = 1, nh
        kh = (h - 1)/(nh/nkv) + 1
        do t = 1, used
            e(t) = sum(q(:, h)*k(:, kh, t))
        end do
        top = maxval(e(1:used))
        e(1:used) = exp(e(1:used) - top)
        total = sum(e(1:used))
        do d = 1, dh
            want = sum(e(1:used)*v(d, kh, 1:used))/total
            if (abs(out(d, h) - want) > 1e-4*max(1.0, abs(want))) &
                error stop "attention"
        end do
    end do
end do
print *, "ok"
end program
