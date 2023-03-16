! The Computer Language Benchmarks Game
! Source: https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-ifc-6.html
!
!   contributed by Simon Geard, translated from  Mark C. Williams nbody.java
!   modified by Brian Taylor
!   modified by yuankun shi
!   modified by Padraig O Conbhui

program nbody
    implicit none

    integer, parameter :: dp = kind(1.d0)

    real(kind=dp), parameter :: tstep = 0.01d0
    real(kind=dp), parameter ::  PI = 3.141592653589793d0
    real(kind=dp), parameter ::  SOLAR_MASS = 4 * PI * PI
    real(kind=dp), parameter ::  DAYS_PER_YEAR = 365.24d0

    type body
        real(kind=dp) :: x, y, z, u, vx, vy, vz, vu, mass
    end type body

    type(body) :: jupiter = body(&
        4.84143144246472090d0, -1.16032004402742839d0, &
        -1.03622044471123109d-01, 0.d0, 1.66007664274403694d-03 * DAYS_PER_YEAR, &
        7.69901118419740425d-03 * DAYS_PER_YEAR, &
        -6.90460016972063023d-05 * DAYS_PER_YEAR, 0.d0,&
        9.54791938424326609d-04 * SOLAR_MASS)
    type(body) :: saturn = body(&
        8.34336671824457987d+00, &
        4.12479856412430479d+00, &
        -4.03523417114321381d-01, 0.d0, &
        -2.76742510726862411d-03 * DAYS_PER_YEAR, &
        4.99852801234917238d-03 * DAYS_PER_YEAR, &
        2.30417297573763929d-05 * DAYS_PER_YEAR, 0.d0,&
        2.85885980666130812d-04 * SOLAR_MASS)
    type(body) :: uranus = body(&
        1.28943695621391310d+01, &
        -1.51111514016986312d+01, &
        -2.23307578892655734d-01, 0.d0,&
        2.96460137564761618d-03 * DAYS_PER_YEAR, &
        2.37847173959480950d-03 * DAYS_PER_YEAR, &
        -2.96589568540237556d-05 * DAYS_PER_YEAR, 0.d0,&
        4.36624404335156298d-05 * SOLAR_MASS )
    type(body) :: neptune = body(&
        1.53796971148509165d+01, &
        -2.59193146099879641d+01, &
        1.79258772950371181d-01, 0.d0,&
        2.68067772490389322d-03 * DAYS_PER_YEAR, &
        1.62824170038242295d-03 * DAYS_PER_YEAR, &
        -9.51592254519715870d-05 * DAYS_PER_YEAR, 0.d0,&
        5.15138902046611451d-05 * SOLAR_MASS)
    type(body) :: sun = body(0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
        0.0d0, 0.d0, 0.d0, SOLAR_MASS)

    integer, parameter :: nb = 5
    integer, parameter :: N = (nb-1)*nb/2

    ! workaround
    real(kind=dp) :: mass(nb)

    integer :: num, i
    character(len=8) :: argv

    real(kind=dp) :: e, x(3,nb), v(3,nb)

    ! workaround
    mass = (/ sun%mass, jupiter%mass, saturn%mass, &
            uranus%mass, neptune%mass /)

    x(:,1) = (/ sun%x, sun%y, sun%z /)
    x(:,2) = (/ jupiter%x, jupiter%y, jupiter%z /)
    x(:,3) = (/ saturn%x, saturn%y, saturn%z /)
    x(:,4) = (/ uranus%x, uranus%y, uranus%z /)
    x(:,5) = (/ neptune%x, neptune%y, neptune%z /)

    v(:,1) = (/ sun%vx, sun%vy, sun%vz /)
    v(:,2) = (/ jupiter%vx, jupiter%vy, jupiter%vz /)
    v(:,3) = (/ saturn%vx, saturn%vy, saturn%vz /)
    v(:,4) = (/ uranus%vx, uranus%vy, uranus%vz /)
    v(:,5) = (/ neptune%vx, neptune%vy, neptune%vz /)

    ! call getarg(1, argv)
    ! read (argv,*) num
    num = 1000

    call offsetMomentum(1, v, mass)
    e = energy(x, v, mass)
    ! workaround
    print *, e
    if (abs(e + 0.16907516382852447) > 1e-8) error stop

    do i = 1, num
        call advance(tstep, x, v, mass)
    end do
    e = energy(x, v, mass)
    ! workaround
    print *, e
    if (abs(e + 0.16908760523460617) > 1e-8) error stop

    contains

   subroutine offsetMomentum(k, v, mass)
        integer, intent(in) :: k
        real(kind=dp), dimension(3,nb), intent(inout) :: v
        real(kind=dp), dimension(nb), intent(in) :: mass

        real(kind=dp), dimension(3) :: p
        integer :: i

        ! workaround
        p(1) = sum(v(1, :) * mass(:))
        p(2) = sum(v(2, :) * mass(:))
        p(3) = sum(v(3, :) * mass(:))
        v(:,k) = -p / SOLAR_MASS
    end subroutine offsetMomentum


    pure subroutine advance(tstep, x, v, mass)
        real(kind=dp), intent(in) :: tstep
        real(kind=dp), dimension(3, nb), intent(inout) :: x, v
        real(kind=dp), dimension(nb), intent(in) :: mass
        real(kind=dp) :: r(3, N), mag(N)

        real(kind=dp) :: distance, d2
        integer :: i, j, m
        m = 1
        do i = 1, nb
            do j = i + 1, nb
                r(:,m) = x(:,i) - x(:,j)
                m = m + 1
            end do
        end do

        do m = 1, N
            d2 = sum(r(:,m)**2)
            distance = 1/sqrt(real(d2))
            distance = distance * (1.5d0 - 0.5d0 * d2 * distance * distance)
            !distance = distance * (1.5d0 - 0.5d0 * d2 * distance * distance)
            mag(m) = tstep * distance**3
        end do

        m = 1
        do i = 1, nb
            do j = i + 1, nb
                v(:,i) = v(:,i) - r(:,m) * mass(j) * mag(m)
                v(:,j) = v(:,j) + r(:,m) * mass(i) * mag(m)

                m = m + 1
            end do
        end do

        x = x + tstep * v
    end subroutine advance


    pure function energy(x, v, mass)
        real(kind=dp) :: energy
        real(kind=dp), dimension(3,nb), intent(in) :: x, v
        real(kind=dp), dimension(nb), intent(in) :: mass

        real(kind=dp) :: distance, tmp
        real(kind=dp), dimension(3) :: d
        integer :: i, j

        energy = 0.0d0
        do i = 1, nb
            energy = energy + 0.5d0 * mass(i) * sum(v(:,i)**2)
            do j = i + 1, nb
                d = x(:,i) - x(:,j)
                tmp = sum(d**2)
                distance = sqrt(tmp)
                energy = energy - (mass(i) * mass(j)) / distance
            end do
        end do
    end function energy

    pure function sum(arr) result(r)
        real(kind=dp), intent(in) :: arr(:)
        real(kind=dp) :: r
        integer :: i
        r = 0.0
        do i = 1, size(arr)
            r = r + arr(i)
        end do
    end function sum

end program nbody
