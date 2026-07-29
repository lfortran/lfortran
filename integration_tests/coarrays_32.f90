program coarrays_32
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer :: x[*]
    integer :: lc(1), uc(1)

    integer(int64), parameter :: lcobound_64 = 4294967296_int64
    integer(int64) :: y[lcobound_64:*]
    integer(int64) :: z[7:7, lcobound_64:*]

    integer(int64) :: lc64_1(1), uc64_1(1)
    integer(int64) :: lc64_2(2), uc64_2(2)
    integer(int64) :: trailing_ucobound

    ! Default kind, corank 1
    lc = lcobound(x)
    uc = ucobound(x)

    if (lc(1) /= 1) error stop "Incorrect LCOBOUND."
    if (uc(1) /= num_images()) error stop "Incorrect UCOBOUND."

    if (lcobound(x, dim=1) /= 1) then
        error stop "Incorrect LCOBOUND with DIM."
    end if

    if (ucobound(x, dim=1) /= num_images()) then
        error stop "Incorrect UCOBOUND with DIM."
    end if

    ! KIND=int64, corank 1
    trailing_ucobound = lcobound_64 + int(num_images() - 1, int64)

    lc64_1 = lcobound(y, kind=int64)
    uc64_1 = ucobound(y, kind=int64)

    if (lc64_1(1) /= lcobound_64) then
        error stop "Incorrect int64 LCOBOUND."
    end if

    if (lcobound(y, dim=1, kind=int64) /= lcobound_64) then
        error stop "Incorrect int64 LCOBOUND with DIM."
    end if

    if (uc64_1(1) /= trailing_ucobound) then
        error stop "Incorrect int64 UCOBOUND."
    end if

    if (ucobound(y, dim=1, kind=int64) /= trailing_ucobound) then
        error stop "Incorrect int64 UCOBOUND with DIM."
    end if

    ! KIND=int64, corank 2
    lc64_2 = lcobound(z, kind=int64)
    uc64_2 = ucobound(z, kind=int64)

    if (lc64_2(1) /= 7_int64) then
        error stop "Incorrect first LCOBOUND."
    end if

    if (lcobound(z, dim=1, kind=int64) /= 7_int64) then
        error stop "Incorrect first LCOBOUND with DIM."
    end if

    if (lc64_2(2) /= lcobound_64) then
        error stop "Incorrect second LCOBOUND."
    end if

    if (lcobound(z, dim=2, kind=int64) /= lcobound_64) then
        error stop "Incorrect second LCOBOUND with DIM."
    end if

    if (uc64_2(1) /= 7_int64) then
        error stop "Incorrect first UCOBOUND."
    end if

    if (ucobound(z, dim=1, kind=int64) /= 7_int64) then
        error stop "Incorrect first UCOBOUND with DIM."
    end if

    if (uc64_2(2) /= trailing_ucobound) then
        error stop "Incorrect second UCOBOUND."
    end if

    if (ucobound(z, dim=2, kind=int64) /= trailing_ucobound) then
        error stop "Incorrect second UCOBOUND with DIM."
    end if

end program coarrays_32