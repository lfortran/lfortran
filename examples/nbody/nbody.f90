! Direct all-pairs gravitational N-body benchmark in single precision.
!
! Every body attracts every other body with softened gravity (G = 1):
!
!     a_i = sum_j m_j (r_j - r_i) / (|r_j - r_i|^2 + eps^2)^(3/2)
!
! The sum includes j = i, whose contribution is zero thanks to the softening,
! so that one step is exactly n**2 interactions, counted as 20 flops each
! (the convention of GPU Gems 3, chapter 31). The time integration is the
! kick-drift-kick leapfrog (velocity Verlet), with one force evaluation per
! step.
!
! The bodies are stored as a structure of arrays: x(:), y(:), z(:) and m(:)
! are contiguous, so a CPU vectorizes the inner loop over j with plain vector
! loads, and on a GPU all threads of a SIMD-group read the same element j at
! the same time, which the hardware broadcasts.
!
! Usage: nbody [n [steps]]
!
! The same source is compiled for one core, for all cores and for the GPU:
!
!     lfortran --fast nbody.f90
!     lfortran --fast --openmp nbody.f90     (do concurrent on all cores)
!     lfortran --fast --gpu=metal nbody.f90  (do concurrent on the GPU)
!     gfortran -O3 -mcpu=native -ffast-math nbody.f90 \
!         [-ftree-parallelize-loops=16]
program nbody
    implicit none
    integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
    integer, parameter :: i8 = selected_int_kind(18)
    real(sp), parameter :: dt = 0.001_sp, eps2 = 0.01_sp
    real(dp), parameter :: flops_per_interaction = 20

    integer :: n, nsteps, i, j, step
    real(sp), allocatable :: x(:), y(:), z(:), vx(:), vy(:), vz(:), m(:)
    real(sp), allocatable :: ax(:), ay(:), az(:)
    real(sp) :: kick, drift
    real(sp) :: xi, yi, zi, axi, ayi, azi, dx, dy, dz, r2, inv_r, s
    integer(i8) :: seed, t0, t1, rate
    real(dp) :: e0, e1, time, interactions, gflops, energy_drift, checksum
    character(len=32) :: arg

    n = 16384
    nsteps = 10
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) n
    end if
    if (command_argument_count() >= 2) then
        call get_command_argument(2, arg)
        read(arg, *) nsteps
    end if
    if (n < 1 .or. nsteps < 1) error stop "usage: nbody [n [steps]]"
    call system_clock(count_rate=rate)

    allocate(x(n), y(n), z(n), vx(n), vy(n), vz(n), m(n))
    allocate(ax(n), ay(n), az(n))

    ! Uniformly random bodies in the cube [-1, 1]^3 with small random
    ! velocities and masses, from a portable generator, so that every
    ! implementation of the benchmark starts from the same bits.
    seed = 42
    do i = 1, n
        x(i) = real(2*uniform(seed) - 1, sp)
        y(i) = real(2*uniform(seed) - 1, sp)
        z(i) = real(2*uniform(seed) - 1, sp)
        vx(i) = real(0.1_dp*(2*uniform(seed) - 1), sp)
        vy(i) = real(0.1_dp*(2*uniform(seed) - 1), sp)
        vz(i) = real(0.1_dp*(2*uniform(seed) - 1), sp)
        m(i) = real((0.5_dp + uniform(seed)) / n, sp)
    end do

    e0 = energy(x, y, z, vx, vy, vz, m)

    ! Step 0 computes the initial accelerations and kicks by half a step, the
    ! last step kicks by the other half, so that the velocities are at the
    ! same time as the positions again. Step 0 is not timed: it also compiles
    ! the GPU kernels.
    t0 = 0
    do step = 0, nsteps
        if (step == 1) then
            ! Reading a result waits for the device before the clock starts.
            checksum = x(1)
            call system_clock(t0)
        end if

        do concurrent (i = 1:n) &
                local(j, xi, yi, zi, axi, ayi, azi, dx, dy, dz, r2, inv_r, s)
            xi = x(i)
            yi = y(i)
            zi = z(i)
            axi = 0
            ayi = 0
            azi = 0
            do j = 1, n
                dx = x(j) - xi
                dy = y(j) - yi
                dz = z(j) - zi
                r2 = dx*dx + dy*dy + dz*dz + eps2
                inv_r = 1 / sqrt(r2)
                s = m(j) * inv_r * inv_r * inv_r
                axi = axi + dx*s
                ayi = ayi + dy*s
                azi = azi + dz*s
            end do
            ax(i) = axi
            ay(i) = ayi
            az(i) = azi
        end do

        if (step == 0 .or. step == nsteps) then
            kick = dt / 2
        else
            kick = dt
        end if
        if (step == nsteps) then
            drift = 0
        else
            drift = dt
        end if
        do concurrent (i = 1:n)
            vx(i) = vx(i) + kick*ax(i)
            vy(i) = vy(i) + kick*ay(i)
            vz(i) = vz(i) + kick*az(i)
            x(i) = x(i) + drift*vx(i)
            y(i) = y(i) + drift*vy(i)
            z(i) = z(i) + drift*vz(i)
        end do
    end do
    ! Reading a result waits for the device before the clock stops.
    checksum = x(1)
    call system_clock(t1)

    time = real(t1 - t0, dp) / real(rate, dp)
    interactions = real(n, dp) * real(n, dp) * nsteps
    e1 = energy(x, y, z, vx, vy, vz, m)
    checksum = 0
    do i = 1, n
        checksum = checksum + abs(x(i)) + abs(y(i)) + abs(z(i))
    end do
    checksum = checksum / n

    gflops = flops_per_interaction * interactions / time / 1e9_dp
    energy_drift = abs((e1 - e0) / e0)
    print "(a, i0, a, i0)", "N = ", n, "  steps = ", nsteps
    print "(a, f10.4, a)", "time = ", time, " s"
    print "(a, es10.3)", "interactions/s = ", interactions / time
    print "(a, f10.1)", "GFLOP/s = ", gflops
    print "(a, es16.8, a, es16.8, a, es9.2)", "energy: E0 = ", e0, &
        "  E1 = ", e1, "  rel. drift = ", energy_drift
    print "(a, f12.8)", "checksum = ", checksum
    ! One line for scripts: n, steps, time [s], GFLOP/s, energy drift, checksum.
    print "(a, i0, 1x, i0, 1x, f12.6, 1x, f10.1, 1x, es9.2, 1x, f12.8)", &
        "RESULT ", n, nsteps, time, gflops, energy_drift, checksum

contains

    ! A uniform number in [0, 1) from a 31-bit linear congruential generator.
    real(dp) function uniform(seed) result(u)
        integer(i8), intent(inout) :: seed
        seed = modulo(1103515245_i8*seed + 12345_i8, 2147483648_i8)
        u = real(seed, dp) / 2147483648.0_dp
    end function

    ! The total (kinetic plus softened potential) energy, in double precision.
    real(dp) function energy(x, y, z, vx, vy, vz, m) result(e)
        real(sp), intent(in) :: x(:), y(:), z(:), vx(:), vy(:), vz(:), m(:)
        real(dp) :: dx, dy, dz, pot
        integer :: i, j
        e = 0
        do i = 1, size(x)
            e = e + 0.5_dp*m(i)*(real(vx(i), dp)**2 + real(vy(i), dp)**2 &
                + real(vz(i), dp)**2)
            pot = 0
            do j = i + 1, size(x)
                dx = real(x(j), dp) - x(i)
                dy = real(y(j), dp) - y(i)
                dz = real(z(j), dp) - z(i)
                pot = pot + m(j) / sqrt(dx*dx + dy*dy + dz*dz + eps2)
            end do
            e = e - m(i)*pot
        end do
    end function

end program
