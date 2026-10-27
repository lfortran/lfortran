program random_number_04
    ! random_number must return values in [0, 1), never exactly 1.0 (#16043).
    !
    ! The real(4) loop uses the default (unseeded) generator, so its sequence
    ! is fixed for a given C library. Before the fix, real(4) results were
    ! rand()/(float)RAND_MAX, which is 1.0 for the 64 largest rand() values
    ! (and for rand() == RAND_MAX on every platform). With LFortran's default
    ! seed the first 1.0 occurs at call 2304450 on macOS, 13068231 on glibc,
    ! 11804170 on musl and within ~1e5 calls on MSVC (RAND_MAX = 32767), so
    ! N = 30000000 catches it on all of them while running well under a
    ! second. After the fix every result is < 1.0 by construction, so the
    ! test cannot fail for any sequence or seed.
    implicit none
    integer, parameter :: n = 30000000
    real :: r, a(1000)
    double precision :: d, b(1000)
    integer :: i

    do i = 1, n
        call random_number(r)
        if (r < 0.0 .or. r >= 1.0) then
            print *, "real(4) out of range:", r, "at call", i
            error stop
        end if
    end do

    do i = 1, 1000000
        call random_number(d)
        if (d < 0.0d0 .or. d >= 1.0d0) then
            print *, "real(8) out of range:", d, "at call", i
            error stop
        end if
    end do

    do i = 1, 1000
        call random_number(a)
        if (any(a < 0.0) .or. any(a >= 1.0)) error stop
        call random_number(b)
        if (any(b < 0.0d0) .or. any(b >= 1.0d0)) error stop
    end do

    print *, "ok"
end program
