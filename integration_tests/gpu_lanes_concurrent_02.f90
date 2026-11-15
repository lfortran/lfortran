module gpu_lanes_concurrent_02_m
! The channels of a head, convolved and then normalized in one kernel: the
! `do concurrent` over the channels of a block assigns an array at subscripts
! that are not the loop variable plus a constant (a modulo, a division, the
! rows of a state shifted by one), which the lanes of the iteration cannot be
! told apart by, but the `do concurrent` says its iterations do not depend on
! each other, so they share the loop between them instead of the first lane
! running it alone (gpu_vector_lanes.cpp).
implicit none
contains
    pure subroutine conv_norm(x, state, w, qk, qn, kdh, heads)
        real, contiguous, intent(in) :: x(:), w(:, :)
        real, contiguous, intent(inout) :: state(:, :)
        real, contiguous, intent(out) :: qk(:, :), qn(:, :)
        integer, intent(in) :: kdh, heads
        integer :: blk
        do concurrent (blk = 1:heads)
            block
                integer :: col, i
                real :: ss, rrms
                col = (blk - 1)/2 + 1 + modulo(blk - 1, 2)*(heads/2)
                do concurrent (i = 1:kdh)
                    block
                        integer :: c, r
                        real :: acc
                        c = (blk - 1)*kdh + i
                        acc = w(1, c)*state(c, 1) + w(2, c)*state(c, 2) &
                            + w(3, c)*x(c)
                        r = modulo(i*5 - 5, kdh)
                        qk(r + 1, col) = acc
                        state(c, 1) = state(c, 2)
                        state(c, 2) = x(c)
                    end block
                end do
                ss = 0
                do concurrent (i = 1:kdh) reduce(+:ss)
                    ss = ss + qk(i, col)**2
                end do
                rrms = 1/sqrt(ss/kdh + 1e-6)
                do concurrent (i = 1:kdh)
                    qn(i, col) = qk(i, col)*rrms
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_concurrent_02
use gpu_lanes_concurrent_02_m
implicit none
integer, parameter :: kdh = 64, heads = 8, n = kdh*heads
real :: x(n), w(3, n), state(n, 2), s0(n, 2), qk(kdh, heads), qn(kdh, heads)
real :: ref(kdh, heads), ss, rrms
integer :: c, blk, col, i, r
do c = 1, n
    x(c) = real(mod(7*c, 13) - 6)/4
    w(:, c) = [real(mod(c, 5)), real(mod(c, 3)), real(mod(c, 7))]/4
    state(c, :) = [real(mod(c, 4)), real(mod(2*c, 9))]/2
end do
s0 = state
call conv_norm(x, state, w, qk, qn, kdh, heads)
do blk = 1, heads
    col = (blk - 1)/2 + 1 + modulo(blk - 1, 2)*(heads/2)
    do i = 1, kdh
        c = (blk - 1)*kdh + i
        r = modulo(i*5 - 5, kdh)
        ref(r + 1, col) = w(1, c)*s0(c, 1) + w(2, c)*s0(c, 2) + w(3, c)*x(c)
        if (state(c, 1) /= s0(c, 2) .or. state(c, 2) /= x(c)) &
            error stop "state"
    end do
end do
do col = 1, heads
    if (any(abs(qk(:, col) - ref(:, col)) > 1e-5)) error stop "qk"
    ss = sum(ref(:, col)**2)
    rrms = 1/sqrt(ss/kdh + 1e-6)
    if (any(abs(qn(:, col) - ref(:, col)*rrms) > 1e-5*(1 + abs(ref(:, col)*rrms)))) &
        error stop "qn"
end do
print *, "ok"
end program
