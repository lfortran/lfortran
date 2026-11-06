! The benchmarks game n-body, restructured after the C++ g++ #0 solution.
!
! The ten pairs are four-element vectors in r(4, PAIRS), the five bodies
! are four-element vectors in x(4, NB) and v(4, NB), the pair indices are
! compile-time constants so the ten pairs unroll, and each component loop
! runs over the four slots (one 256-bit operation). Derived from the Intel
! Fortran #5 source (nbody.ifx-5.f90).

program nbody
  implicit none
  integer, parameter :: nb = 5, npair = nb*(nb-1)/2
  real(8), parameter :: tstep = 0.01d0
  real(8), parameter :: pi = 3.141592653589793d0
  real(8), parameter :: solar_mass = 4 * pi * pi
  real(8), parameter :: days_per_year = 365.24d0

  real(8) :: x(4,nb), v(4,nb), m(nb)
  real(8) :: e0, e1
  integer, parameter :: pair_i(npair) = [2,3,3,4,4,4,5,5,5,5], pair_j(npair) = [1,1,2,1,2,3,1,2,3,4]
  integer :: n, step
  character(len=32) :: arg

  call init(x, v, m)

  call get_command_argument(1, arg)
  read(arg, *) n

  e0 = energy(x, v, m)
  write (*,'(f12.9)') e0
  do step = 1, n
     call advance(x, v, m)
  end do
  e1 = energy(x, v, m)
  write (*,'(f12.9)') e1

contains

  subroutine init(x, v, m)
    real(8), intent(out) :: x(4,nb), v(4,nb), m(nb)
    real(8) :: p(4,nb), u(4,nb), o(4)
    integer :: i

    p(:,1) = [0d0, 0d0, 0d0, 0d0]
    u(:,1) = [0d0, 0d0, 0d0, 0d0]
    m(1) = solar_mass

    p(:,2) = [4.84143144246472090d0, -1.16032004402742839d0, &
      -1.03622044471123109d-01, 0d0]
    u(:,2) = [1.66007664274403694d-03 * days_per_year, &
      7.69901118419740425d-03 * days_per_year, &
      -6.90460016972063023d-05 * days_per_year, 0d0]
    m(2) = 9.54791938424326609d-04 * solar_mass

    p(:,3) = [8.34336671824457987d0, 4.12479856412430479d0, &
      -4.03523417114321381d-01, 0d0]
    u(:,3) = [-2.76742510726862411d-03 * days_per_year, &
      4.99852801234917238d-03 * days_per_year, &
      2.30417297573763929d-05 * days_per_year, 0d0]
    m(3) = 2.85885980666130812d-04 * solar_mass

    p(:,4) = [1.28943695621391310d+01, -1.51111514016986312d+01, &
      -2.23307578892655734d-01, 0d0]
    u(:,4) = [2.96460137564761618d-03 * days_per_year, &
      2.37847173959480950d-03 * days_per_year, &
      -2.96589568540237556d-05 * days_per_year, 0d0]
    m(4) = 4.36624404335156298d-05 * solar_mass

    p(:,5) = [1.53796971148509165d+01, -2.59193146099879641d+01, &
      1.79258772950371181d-01, 0d0]
    u(:,5) = [2.68067772490389322d-03 * days_per_year, &
      1.62824170038242295d-03 * days_per_year, &
      -9.51592254519715870d-05 * days_per_year, 0d0]
    m(5) = 5.15138902046611451d-05 * solar_mass

    o = 0d0
    do i = 1, nb
       o = o + m(i) * u(:,i)
    end do
    u(:,1) = o * (-1d0 / solar_mass)
    x = p
    v = u
  end subroutine init

  subroutine advance(x, v, m)
    real(8), intent(inout) :: x(4,nb), v(4,nb)
    real(8), intent(in) :: m(nb)
    real(8) :: r(4,npair), w(npair)
    real(8) :: d2
    integer :: i, j, k, d

    do k = 1, npair
       do d = 1, 4
          r(d,k) = x(d,pair_i(k)) - x(d,pair_j(k))
       end do
    end do

    do k = 1, npair
       d2 = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
       w(k) = 1 / sqrt(real(d2))
       w(k) = w(k) * (1.5d0 - 0.5d0 * d2 * w(k) * w(k))
       w(k) = tstep * w(k) * w(k) * w(k)
    end do

    do k = 1, npair
       do d = 1, 4
          v(d,pair_i(k)) = v(d,pair_i(k)) - r(d,k) * (m(pair_j(k)) * w(k))
          v(d,pair_j(k)) = v(d,pair_j(k)) + r(d,k) * (m(pair_i(k)) * w(k))
       end do
    end do

    do i = 1, nb
       do d = 1, 4
          x(d,i) = x(d,i) + tstep * v(d,i)
       end do
    end do
  end subroutine advance

  real(8) function energy(x, v, m) result(e)
    real(8), intent(in) :: x(4,nb), v(4,nb), m(nb)
    real(8) :: r(4,npair), w(npair), d2, pot
    integer :: i, j, k, d

    e = 0d0
    do i = 1, nb
       pot = 0d0
       do d = 1, 3
          pot = pot + v(d,i) * v(d,i)
       end do
       e = e + 0.5d0 * m(i) * pot
    end do

    do k = 1, npair
       do d = 1, 4
          r(d,k) = x(d,pair_i(k)) - x(d,pair_j(k))
       end do
    end do
    do k = 1, npair
       d2 = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
       w(k) = 1 / sqrt(d2)
    end do
    k = 0
    do i = 2, nb
       do j = 1, i - 1
          k = k + 1
          e = e - m(i) * m(j) * w(k)
       end do
    end do
  end function energy

end program nbody
