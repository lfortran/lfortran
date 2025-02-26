program flip_sign
    implicit none
    real :: rxsp = 5.5, epsrsp = 1e-6
    real(8) :: rxdp = 5.5, epsrdp = 1e-6
    integer :: ixsp = 5, epsisp = 16
    integer(8) :: ixdp = 5, epsidp = 16
    integer :: a=2, b=-3, c
    real :: x=-2, y=-3, z, arr(5)

    rxsp = rxsp * sign(1._4, epsrsp)
    print *, rxsp
    if (abs(rxsp - 5.5) > epsrsp) error stop

    rxsp = rxsp * sign(1._4, -epsrsp)
    print *, rxsp
    if (abs(rxsp + 5.5) > epsrsp) error stop

    rxdp = rxdp * sign(1._8, epsrdp)
    print *, rxdp
    if (abs(rxdp - 5.5) > epsrdp) error stop

    ixsp = ixsp * sign(1_4, epsisp)
    print *, ixsp
    if (ixsp /= 5) error stop

    rxdp = rxdp * sign(1._8, -epsrdp)
    print *, rxdp
    if (abs(rxdp + 5.5) > epsrdp) error stop

    ixdp = ixdp * sign(1_8, epsidp) ! Test that we don't apply sign opt. on integers.
    print *, ixdp
    if (ixdp /= 5) error stop

    c = a*sign(1, b) ! Test that we don't apply sign opt. on integers.
    print *, c
    if(c /= -2) error stop

    arr = [1.0, 2.0, 3.0, 4.0, 5.0]
    z = arr(int(x*sign(1.0,y), 4)) ! Test nested `sign` expression
    print *, z
    if(z /= arr(2)) error stop

end program
