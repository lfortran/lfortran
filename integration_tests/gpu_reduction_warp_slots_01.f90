program gpu_reduction_warp_slots_01
! A reduction on an NVIDIA GPU runs a grid-stride loop on many threads, and
! the threads of a warp combine their accumulators before the warp writes one
! slot for the host to fold (GpuDeviceCapabilities::reduction_warp_slots).
! The sizes are around a warp, a block and the number of threads of a launch,
! so that warps run partly or not at all; the results are compared with
! the same loops on the host.
implicit none
integer, parameter :: dp = kind(0.d0)
integer :: sizes(9), k, n, i, j
real(dp), allocatable :: a(:), b(:), m(:, :)
real(4), allocatable :: x(:)
integer, allocatable :: ia(:)
real(dp) :: s, mx, mn, rs, rmx, rmn, t2, rt2
real(4) :: fs, frs, fmx, frmx
integer :: isum, imax, imin, irs, irmx, irmn

sizes = [1, 2, 31, 32, 33, 1000, 65537, 262145, 1000003]
do k = 1, size(sizes)
    n = sizes(k)
    allocate(a(n), b(n), x(n), ia(n))
    do i = 1, n
        a(i) = sin(real(i, dp))
        b(i) = 1 + mod(i, 7)
        x(i) = real(mod(i*3, 17) - 8)
        ia(i) = mod(i*7, 101) - 50
    end do

    s = 0; mx = -huge(1.0_dp); mn = huge(1.0_dp)
    do concurrent (i = 1:n) reduce(+:s) reduce(max:mx) reduce(min:mn)
        s = s + a(i)*b(i)
        mx = max(mx, a(i))
        mn = min(mn, a(i))
    end do
    rs = 0; rmx = -huge(1.0_dp); rmn = huge(1.0_dp)
    do i = 1, n
        rs = rs + a(i)*b(i)
        rmx = max(rmx, a(i))
        rmn = min(rmn, a(i))
    end do
    if (abs(s - rs) > 1e-9_dp*max(1.0_dp, abs(rs))*sqrt(real(n, dp))) then
        print *, "sum", n, s, rs
        error stop 1
    end if
    if (mx /= rmx .or. mn /= rmn) then
        print *, "max/min", n, mx, rmx, mn, rmn
        error stop 2
    end if

    isum = 0; imax = -huge(1); imin = huge(1)
    do concurrent (i = 1:n) reduce(+:isum) reduce(max:imax) reduce(min:imin)
        isum = isum + ia(i)
        imax = max(imax, ia(i))
        imin = min(imin, ia(i))
    end do
    irs = 0; irmx = -huge(1); irmn = huge(1)
    do i = 1, n
        irs = irs + ia(i)
        irmx = max(irmx, ia(i))
        irmn = min(irmn, ia(i))
    end do
    if (isum /= irs .or. imax /= irmx .or. imin /= irmn) then
        print *, "integer", n, isum, irs, imax, irmx, imin, irmn
        error stop 3
    end if

    ! The values are small integers, so every partial sum is exact.
    fs = 0; fmx = -huge(1.0)
    do concurrent (i = 1:n) reduce(+:fs) reduce(max:fmx)
        fs = fs + x(i)
        fmx = max(fmx, x(i))
    end do
    frs = 0; frmx = -huge(1.0)
    do i = 1, n
        frs = frs + x(i)
        frmx = max(frmx, x(i))
    end do
    if (fs /= frs .or. fmx /= frmx) then
        print *, "real(4)", n, fs, frs, fmx, frmx
        error stop 4
    end if
    deallocate(a, b, x, ia)
end do

! A reduction over a two-dimensional nest.
allocate(m(300, 70))
do j = 1, 70
    do i = 1, 300
        m(i, j) = real(i + 2*j, dp)
    end do
end do
t2 = 0
do concurrent (j = 1:70, i = 1:300) reduce(+:t2)
    t2 = t2 + m(i, j)
end do
rt2 = sum(m)
if (t2 /= rt2) then
    print *, "2d", t2, rt2
    error stop 5
end if
print *, "ok"
end program
