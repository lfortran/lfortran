module gpu_block_shared_06_m
implicit none
integer, parameter :: hd = 64, g = 3, ac = 8

contains

! The attention of a new token, split by key head and chunk of positions
! (flash decoding), on the lanes of a block per chunk (gpu_vector_lanes.cpp):
! the norms of the query heads and of the key, each on a warp of lanes; the
! normalized vectors in the shared memory of the block, at two subscript
! lists that both tell the lanes apart; the scores of the chunk's positions,
! each position on a warp whose lanes take every 32nd element and combine
! with shuffles, inside the loop over the query heads; the softmax within
! the chunk, and the weighted sum of its values with the values read once
! for all heads. The chunk with the new position appends its key and value.
subroutine attn_part(y, eps, p, kc, vc, pm, pl, pa)
    real, intent(in) :: y(:), eps
    integer, intent(in) :: p
    real, intent(inout) :: kc(:,:,:), vc(:,:,:), pm(:,:,:), pl(:,:,:), pa(:,:,:,:)
    integer :: nkv, nq, kh, c
    nkv = size(kc, 2)
    nq = g*nkv
    do concurrent (kh = 1:nkv, c = 1:p/ac + 1)
        block
            real :: x(hd, g+1), w(ac, g), nrm(g+1)
            real :: ss, s, m, l, sc
            integer :: i, j, d, u, t, b
            do concurrent (i = 1:g+1)
                b = merge(((kh-1)*g + i - 1)*hd, nq*hd + (kh-1)*hd, i <= g)
                ss = 0
                do j = 1, hd
                    ss = ss + y(b+j)**2
                end do
                nrm(i) = 1 / sqrt(ss/hd + eps)
            end do
            do concurrent (d = 1:hd)
                if (d <= 8) then
                    sc = 2
                else
                    sc = 1
                end if
                do i = 1, g
                    x(d,i) = y(((kh-1)*g + i - 1)*hd + d)*nrm(i)*sc/8
                end do
                x(d,g+1) = y(nq*hd + (kh-1)*hd + d)*nrm(g+1)
            end do
            if (c == p/ac + 1) then
                do concurrent (d = 1:hd)
                    kc(d,kh,p+1) = x(d,g+1)
                    vc(d,kh,p+1) = y(nq*hd + nkv*hd + (kh-1)*hd + d)
                end do
            end if
            do concurrent (u = 1:ac)
                t = min((c-1)*ac + u, p+1)
                do i = 1, g
                    s = 0
                    do d = 1, hd
                        s = s + x(d,i)*kc(d,kh,t)
                    end do
                    w(u,i) = s
                end do
            end do
            do concurrent (i = 1:g)
                m = -huge(1.0)
                do u = 1, ac
                    m = max(m, merge(w(u,i), -huge(1.0), (c-1)*ac + u <= p+1))
                end do
                l = 0
                do u = 1, ac
                    w(u,i) = merge(exp(w(u,i) - m), 0.0, (c-1)*ac + u <= p+1)
                    l = l + w(u,i)
                end do
                pm(i,kh,c) = m
                pl(i,kh,c) = l
            end do
            do concurrent (d = 1:hd)
                block
                    real :: v(ac)
                    do u = 1, ac
                        v(u) = vc(d,kh,min((c-1)*ac + u, p+1))
                    end do
                    do i = 1, g
                        s = 0
                        do u = 1, ac
                            s = s + w(u,i)*v(u)
                        end do
                        pa(d,i,kh,c) = s
                    end do
                end block
            end do
        end block
    end do
end subroutine

! The chunks merged, by query head: the weight of each chunk in the shared
! memory of the block, from sums over the chunks that read across the
! columns of `pm` and `pl`, then the sums over the chunks by dimension, in
! a loop of a number of iterations known only when the kernel runs.
subroutine attn_combine(pm, pl, pa, p, out)
    real, intent(in) :: pm(:,:,:), pl(:,:,:), pa(:,:,:,:)
    integer, intent(in) :: p
    real, intent(inout) :: out(:)
    integer, parameter :: maxch = 64
    integer :: nkv, h
    nkv = size(pm, 2)
    do concurrent (h = 1:g*nkv)
        block
            real :: f(maxch)
            real :: mm, den, num
            integer :: c, i, kh, d
            kh = (h-1)/g + 1
            i = h - (kh-1)*g
            mm = -huge(1.0)
            do c = 1, p/ac + 1
                mm = max(mm, pm(i,kh,c))
            end do
            den = 0
            do c = 1, p/ac + 1
                den = den + exp(pm(i,kh,c) - mm)*pl(i,kh,c)
            end do
            do concurrent (c = 1:p/ac + 1)
                f(c) = exp(pm(i,kh,c) - mm)/den
            end do
            do concurrent (d = 1:hd)
                num = 0
                do c = 1, p/ac + 1
                    num = num + f(c)*pa(d,i,kh,c)
                end do
                out((h-1)*hd + d) = num
            end do
        end block
    end do
end subroutine

end module

program gpu_block_shared_06
use gpu_block_shared_06_m
implicit none
integer, parameter :: nkv = 2, nq = g*nkv, ctx = 200
real :: y(nq*hd + 2*nkv*hd), kc(hd, nkv, ctx), vc(hd, nkv, ctx), out(nq*hd)
real :: pm(g, nkv, ctx/ac + 1), pl(g, nkv, ctx/ac + 1), pa(hd, g, nkv, ctx/ac + 1)
real :: q(hd), kn(hd), sco(ctx), ref, err, m, ss, sc
integer :: i, j, t, kh, h, d, p, steps(3), k

do i = 1, size(y)
    y(i) = 0.1*sin(0.37*i)
end do
do t = 1, ctx
    do kh = 1, nkv
        do d = 1, hd
            kc(d,kh,t) = 0.05*cos(0.13*d + 0.7*kh + 0.011*t)
            vc(d,kh,t) = 0.05*sin(0.17*d + 0.3*kh + 0.013*t)
        end do
    end do
end do
steps = [0, 9, 150]
err = 0
do k = 1, size(steps)
    p = steps(k)
    call attn_part(y, 1e-6, p, kc, vc, pm, pl, pa)
    call attn_combine(pm, pl, pa, p, out)
    do h = 1, nq
        kh = (h-1)/g + 1
        ss = sum(y((h-1)*hd+1:h*hd)**2)
        do d = 1, hd
            sc = merge(2.0, 1.0, d <= 8)
            q(d) = y((h-1)*hd + d)/sqrt(ss/hd + 1e-6)*sc/8
        end do
        ss = sum(y(nq*hd + (kh-1)*hd + 1:nq*hd + kh*hd)**2)
        kn = y(nq*hd + (kh-1)*hd + 1:nq*hd + kh*hd)/sqrt(ss/hd + 1e-6)
        if (any(abs(kc(:,kh,p+1) - kn) > 1e-5)) error stop 1
        if (any(vc(:,kh,p+1) /= y(nq*hd + nkv*hd + (kh-1)*hd + 1:nq*hd + nkv*hd + kh*hd))) error stop 2
        do t = 1, p + 1
            sco(t) = dot_product(q, kc(:,kh,t))
        end do
        m = maxval(sco(1:p+1))
        do d = 1, hd
            ref = 0
            do t = 1, p + 1
                ref = ref + exp(sco(t) - m)*vc(d,kh,t)
            end do
            ref = ref/sum(exp(sco(1:p+1) - m))
            err = max(err, abs(out((h-1)*hd + d) - ref))
        end do
    end do
end do
print *, "max error", err
if (err > 1e-5) error stop 3
print *, "ok"
end program
