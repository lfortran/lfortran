module gpu_residue_unroll_02_m
    implicit none
contains
    pure elemental integer function part(word, idx)
        integer, intent(in) :: word, idx
        if (idx < 0 .or. idx > 3) error stop "part requires idx in 0..3"
        part = ibits(word, 8*idx, 8)
    end function

    ! Loops over packed words with bounds known only at run time: empty,
    ! shorter than a group, starting between words, and not a whole number
    ! of groups long. The loop variable is read after the loop.
    pure subroutine unaligned(w, lo, hi, s, last)
        integer, intent(in) :: w(0:), lo(:), hi(:)
        integer, intent(out) :: s(:), last(:)
        integer :: n
        do concurrent (n = 1:size(s))
            block
                integer :: d, acc
                acc = 0
                do d = lo(n), hi(n)
                    acc = acc + part(w((d-1)/4), modulo(d-1, 4))*d
                end do
                s(n) = acc
                last(n) = d
            end block
        end do
    end subroutine

    ! Numerators that are negative for some or all iterations, where
    ! the truncating quotient and `mod` differ from `modulo`, and one
    ! with an odd offset.
    pure subroutine signed(lo, hi, s, t)
        integer, intent(in) :: lo(:), hi(:)
        integer, intent(out) :: s(:), t(:)
        integer :: n
        do concurrent (n = 1:size(s))
            block
                integer :: d, acc, acc2
                acc = 0
                acc2 = 0
                do d = lo(n), hi(n)
                    acc = acc + 100*((d - 20)/4) + 10*mod(d - 20, 4) &
                        + modulo(d - 20, 4)
                end do
                do d = lo(n), hi(n)
                    acc2 = acc2 + 7*((d + 3)/4) + modulo(d + 3, 4)*d
                end do
                s(n) = acc
                t(n) = acc2
            end block
        end do
    end subroutine

    ! A word the body writes is read again in each iteration.
    pure subroutine counts(lo, hi, s)
        integer, intent(in) :: lo(:), hi(:)
        integer, intent(out) :: s(:)
        integer :: n
        do concurrent (n = 1:size(s))
            block
                integer :: d, j, acc, cnt(0:40)
                cnt = 0
                do d = lo(n), hi(n)
                    cnt((d-1)/4) = cnt((d-1)/4)*3 + modulo(d-1, 4) + 1
                end do
                acc = 0
                do j = 0, 40
                    acc = acc + cnt(j)*(j + 1)
                end do
                s(n) = acc
            end block
        end do
    end subroutine
end module

program gpu_residue_unroll_02
    use gpu_residue_unroll_02_m, only: unaligned, signed, counts
    implicit none
    integer, parameter :: m = 64
    integer :: w(0:300), lo(m), hi(m), s(m), t(m), last(m)
    integer :: i, n, d, e, e2, cnt(0:40)
    do i = 0, 300
        w(i) = mod(i*37, 128)*16777216 + mod(i*11, 256)*65536 &
            + mod(i*5 + 3, 256)*256 + mod(i*29 + 1, 256)
    end do

    do n = 1, m
        lo(n) = mod(n*7, 13) + 1
        hi(n) = lo(n) + mod(n*5, 23) - 3
    end do
    call unaligned(w, lo, hi, s, last)
    do n = 1, m
        e = 0
        do d = lo(n), hi(n)
            e = e + ibits(w((d-1)/4), 8*modulo(d-1, 4), 8)*d
        end do
        if (s(n) /= e) error stop "unaligned"
        if (last(n) /= d) error stop "loop variable after the loop"
    end do

    do n = 1, m
        lo(n) = n - 30
        hi(n) = lo(n) + mod(n, 11) + 2
    end do
    call signed(lo, hi, s, t)
    do n = 1, m
        e = 0
        e2 = 0
        do d = lo(n), hi(n)
            e = e + 100*((d - 20)/4) + 10*mod(d - 20, 4) + modulo(d - 20, 4)
            e2 = e2 + 7*((d + 3)/4) + modulo(d + 3, 4)*d
        end do
        if (s(n) /= e) error stop "signed"
        if (t(n) /= e2) error stop "odd offset"
    end do

    do n = 1, m
        lo(n) = mod(n*3, 9) + 1
        hi(n) = lo(n) + mod(n*13, 150)
    end do
    call counts(lo, hi, s)
    do n = 1, m
        cnt = 0
        do d = lo(n), hi(n)
            cnt((d-1)/4) = cnt((d-1)/4)*3 + modulo(d-1, 4) + 1
        end do
        e = 0
        do i = 0, 40
            e = e + cnt(i)*(i + 1)
        end do
        if (s(n) /= e) error stop "counts"
    end do
    print *, "ok"
end program
