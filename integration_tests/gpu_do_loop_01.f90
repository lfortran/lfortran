! Counted DO loops inside device code: the device code generators write
! them as C `for` loops, which must give each loop Fortran's trip count and
! leave its index where Fortran leaves it.
module gpu_do_loop_01_m
    implicit none
contains

    ! The end bound is an intent(in) dummy, which the loop cannot change.
    pure integer function triangle(n) result(s)
        integer, intent(in) :: n
        integer :: k
        s = 0
        do k = 1, n
            s = s + k
        end do
    end function

end module

program gpu_do_loop_01
    use gpu_do_loop_01_m, only: triangle
    implicit none
    integer, parameter :: n = 20, ncols = 9
    integer :: r(ncols, n), lim(n), expected(ncols), i, jj, kk, ss, tt

    do i = 1, n
        lim(i) = mod(7*i, 11) - 2
    end do

    do concurrent (i = 1:n)
        block
            integer :: j, k, m, s, t
            ! A negative step.
            t = 0
            do j = i, 1, -3
                t = t + j
            end do
            r(1, i) = t*100 + j
            ! A positive step other than one.
            t = 0
            do j = 1, i, 4
                t = t + j
            end do
            r(2, i) = t*100 + j
            ! The end bound changes in the body: it is evaluated once.
            t = 0
            m = i
            do j = 1, m
                m = m - 1
                t = t + 1
            end do
            r(3, i) = t*100 + j
            ! A bound that is an intent(in) dummy of a device routine.
            r(4, i) = triangle(i)
            ! EXIT and CYCLE of an outer loop from an inner one.
            t = 0
            outer: do j = 1, 5
                do k = 1, 5
                    if (k > j) cycle outer
                    if (j == mod(i, 4) + 2) exit outer
                    t = t + k
                end do
            end do outer
            r(5, i) = t*100 + j*10 + k
            ! Loops that run no iteration leave the index at the start.
            do j = i + 1, i
                t = -1
            end do
            do k = 1, 5, -1
                t = -1
            end do
            r(6, i) = j*100 + k
            ! A step whose sign is only known when the loop runs.
            s = merge(2, -2, mod(i, 2) == 0)
            t = 0
            do j = merge(1, 10, s > 0), merge(10, 1, s > 0), s
                t = t + j
            end do
            r(7, i) = t*100 + j
            ! The step changes in the body: it is evaluated once.
            s = 1 + mod(i, 3)
            t = 0
            do j = 1, 10, s
                s = s + 1
                t = t + j
            end do
            r(8, i) = t*100 + j
            ! A bound that is an array element.
            t = 0
            do j = 1, lim(i)
                t = t + j
            end do
            r(9, i) = t*100 + j
        end block
    end do

    ! The expected values, counted with DO WHILE loops.
    do i = 1, n
        tt = 0
        jj = i
        do while (jj >= 1)
            tt = tt + jj
            jj = jj - 3
        end do
        expected(1) = tt*100 + jj
        tt = 0
        jj = 1
        do while (jj <= i)
            tt = tt + jj
            jj = jj + 4
        end do
        expected(2) = tt*100 + jj
        expected(3) = i*100 + i + 1
        expected(4) = i*(i + 1)/2
        kk = mod(i, 4) + 2
        expected(5) = (kk - 1)*kk*(kk + 1)/6*100 + kk*10 + 1
        expected(6) = (i + 1)*100 + 1
        if (mod(i, 2) == 0) then
            expected(7) = 25*100 + 11
        else
            expected(7) = 30*100 + 0
        end if
        ss = 1 + mod(i, 3)
        tt = 0
        jj = 1
        do while (jj <= 10)
            tt = tt + jj
            jj = jj + ss
        end do
        expected(8) = tt*100 + jj
        tt = 0
        jj = 1
        do while (jj <= lim(i))
            tt = tt + jj
            jj = jj + 1
        end do
        expected(9) = tt*100 + jj
        do kk = 1, ncols
            if (r(kk, i) /= expected(kk)) then
                print *, "i =", i, "column", kk, ":", r(kk, i), "/=", &
                    expected(kk)
                error stop
            end if
        end do
    end do
    print *, "ok"
end program
