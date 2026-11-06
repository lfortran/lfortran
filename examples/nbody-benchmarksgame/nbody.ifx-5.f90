
! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
!   contributed by Simon Geard, translated from  Mark C. Williams nbody.java
!   modified by Brian Taylor
!   modified by yuankun shi

program nbody
  implicit none

  real*8, parameter :: tstep = 0.01d0
  real*8, parameter ::  PI = 3.141592653589793d0
  real*8, parameter ::  SOLAR_MASS = 4 * PI * PI
  real*8, parameter ::  DAYS_PER_YEAR = 365.24d0

  type body
     real*8 :: x, y, z, u, vx, vy, vz, vu, mass
  end type body

  type(body), parameter :: jupiter = body( &
       4.84143144246472090d0,    -1.16032004402742839d0, &
       -1.03622044471123109d-01, 0.d0, 1.66007664274403694d-03 * DAYS_PER_YEAR, &
       7.69901118419740425d-03 * DAYS_PER_YEAR, &
       -6.90460016972063023d-05 * DAYS_PER_YEAR, 0.d0,&
       9.54791938424326609d-04 * SOLAR_MASS)

  type(body), parameter :: saturn = body( &
       8.34336671824457987d+00, &
       4.12479856412430479d+00, &
       -4.03523417114321381d-01, 0.d0, &
       -2.76742510726862411d-03 * DAYS_PER_YEAR, &
       4.99852801234917238d-03 * DAYS_PER_YEAR, &
       2.30417297573763929d-05 * DAYS_PER_YEAR, 0.d0,&
       2.85885980666130812d-04 * SOLAR_MASS)

  type(body), parameter :: uranus = body( &
       1.28943695621391310d+01, &
       -1.51111514016986312d+01, &
       -2.23307578892655734d-01, 0.d0,&
       2.96460137564761618d-03 * DAYS_PER_YEAR, &
       2.37847173959480950d-03 * DAYS_PER_YEAR, &
       -2.96589568540237556d-05 * DAYS_PER_YEAR, 0.d0,&
       4.36624404335156298d-05 * SOLAR_MASS )

  type(body), parameter :: neptune = body( &
       1.53796971148509165d+01, &
       -2.59193146099879641d+01, &
       1.79258772950371181d-01, 0.d0,&
       2.68067772490389322d-03 * DAYS_PER_YEAR, &
       1.62824170038242295d-03 * DAYS_PER_YEAR, &
       -9.51592254519715870d-05 * DAYS_PER_YEAR, 0.d0,&
       5.15138902046611451d-05 * SOLAR_MASS)

  type(body), parameter :: sun = body(0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
        0.0d0, 0.d0, 0.d0, SOLAR_MASS)

  integer, parameter :: nb = 5
  integer, parameter :: N = (nb-1)*nb/2 

  real*8, parameter :: mass(nb) = (/ sun%mass, jupiter%mass, saturn%mass, &
        uranus%mass, neptune%mass /)

  integer :: num, i
  character(len=8) :: argv

  real*8 :: e, x(4,nb), v(4,nb)

  x(1:3,1) = (/ sun%x, sun%y, sun%z /)
  x(1:3,2) = (/ jupiter%x, jupiter%y, jupiter%z /)
  x(1:3,3) = (/ saturn%x, saturn%y, saturn%z /)
  x(1:3,4) = (/ uranus%x, uranus%y, uranus%z /)
  x(1:3,5) = (/ neptune%x, neptune%y, neptune%z /)

  v(1:3,1) = (/ sun%vx, sun%vy, sun%vz /)
  v(1:3,2) = (/ jupiter%vx, jupiter%vy, jupiter%vz /)
  v(1:3,3) = (/ saturn%vx, saturn%vy, saturn%vz /)
  v(1:3,4) = (/ uranus%vx, uranus%vy, uranus%vz /)
  v(1:3,5) = (/ neptune%vx, neptune%vy, neptune%vz /)

  call getarg(1, argv)
  read (argv,*) num

  call offsetMomentum(1, v, mass)
  e = energy(x, v, mass)
  write (*,'(f12.9)') e
  do i = 1, num
     call advance(tstep, x, v, mass)
  end do
  e = energy(x, v, mass)
  write (*,'(f12.9)') e

contains

  pure subroutine offsetMomentum(k, v, mass)
    integer, intent(in) :: k
    real*8, dimension(4,nb), intent(inout) :: v
    real*8, dimension(nb), intent(in) :: mass
    real*8 :: px, py, pz
    integer :: i
    px = 0.0d0
    py = 0.0d0
    pz = 0.0d0
    do i = 1, nb
       px = px + v(1,i) * mass(i)
       py = py + v(2,i) * mass(i)
       pz = pz + v(3,i) * mass(i)
    end do
    v(1,k) = -px / SOLAR_MASS
    v(2,k) = -py / SOLAR_MASS
    v(3,k) = -pz / SOLAR_MASS
  end subroutine offsetMomentum


  pure subroutine advance(tstep, x, v, mass)
  real*8, intent(in) :: tstep
  real*8, dimension(4,nb), intent(inout) :: x, v
  real*8, dimension(nb), intent(in) :: mass
  real*8 :: r(4,N),mag(N)

  real*8 :: distance, d2
  integer :: i, j, m
  m = 1
  do i = 1, nb
     do j = i + 1, nb
        r(1,m) = x(1,i) - x(1,j)
        r(2,m) = x(2,i) - x(2,j)
        r(3,m) = x(3,i) - x(3,j)
        m = m + 1
     end do
  end do
  
  do m = 1, N
     d2 = r(1,m)**2 + r(2,m)**2 + r(3,m)**2
     distance = 1/sqrt(real(d2))
     distance = distance * (1.5d0 - 0.5d0 * d2 * distance * distance)
     !distance = distance * (1.5d0 - 0.5d0 * d2 * distance * distance)
     mag(m) = tstep * distance**3
  end do

  m = 1
  do i = 1, nb
     do j = i + 1, nb
        v(1,i) = v(1,i) - r(1,m) * mass(j) * mag(m)
        v(2,i) = v(2,i) - r(2,m) * mass(j) * mag(m)
        v(3,i) = v(3,i) - r(3,m) * mass(j) * mag(m)

        v(1,j) = v(1,j) + r(1,m) * mass(i) * mag(m)
        v(2,j) = v(2,j) + r(2,m) * mass(i) * mag(m)
        v(3,j) = v(3,j) + r(3,m) * mass(i) * mag(m)

        m = m + 1
     end do
  end do
  do i = 1, nb
     x(1,i) = x(1,i) + tstep * v(1,i)
     x(2,i) = x(2,i) + tstep * v(2,i)
     x(3,i) = x(3,i) + tstep * v(3,i)
  end do
  end subroutine advance


  pure function energy(x, v, mass)
    real*8 :: energy
    real*8, dimension(4,nb), intent(in) :: x, v
    real*8, dimension(nb), intent(in) :: mass

    real*8 :: dx, dy, dz, distance
    integer :: i, j

    energy = 0.0d0
    do i = 1, nb
       energy = energy + 0.5d0 * mass(i) * (v(1,i)**2 + v(2,i)**2 + v(3,i)**2)
       do j = i + 1, nb
          dx = x(1,i) - x(1,j)
          dy = x(2,i) - x(2,j)
          dz = x(3,i) - x(3,j)
          distance = sqrt(dx**2 + dy**2 + dz**2)
          energy = energy - (mass(i) * mass(j)) / distance;
       end do
    end do
  end function energy

end program nbody
    