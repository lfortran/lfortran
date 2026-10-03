module struct_type_13_mod
    implicit none
    type :: basis_type
        real :: lat(3,3) = 0.0
        real :: vec(3)   = 0.0
    end type basis_type
contains
    function make_lat(abc, angle, radians) result(lattice)
        real, dimension(3), intent(in) :: abc, angle
        logical, intent(in), optional :: radians
        real, dimension(3,3) :: lattice
        lattice = 0.0
        lattice(1,1) = abc(1)
        lattice(2,2) = abc(2)
        lattice(3,3) = abc(3)
        if (present(radians)) then
            if (radians) lattice(1,2) = angle(1)
        end if
    end function make_lat

    function make_vec(scale) result(v)
        real, intent(in) :: scale
        real :: v(3)
        v = [scale, scale*2.0, scale*3.0]
    end function make_vec
end module struct_type_13_mod

program struct_type_13
    use struct_type_13_mod
    implicit none
    type(basis_type) :: basis
    real :: abc(3) = [1.0, 2.0, 3.0]
    real :: angle(3) = [10.0, 20.0, 30.0]

    basis%lat = make_lat(abc, angle, .false.)
    if (basis%lat(1,1) /= 1.0) error stop "lat(1,1) wrong"
    if (basis%lat(2,2) /= 2.0) error stop "lat(2,2) wrong"
    if (basis%lat(3,3) /= 3.0) error stop "lat(3,3) wrong"
    if (basis%lat(1,2) /= 0.0) error stop "lat(1,2) wrong"

    basis%vec = make_vec(0.5)
    if (basis%vec(1) /= 0.5) error stop "vec(1) wrong"
    if (basis%vec(2) /= 1.0) error stop "vec(2) wrong"
    if (basis%vec(3) /= 1.5) error stop "vec(3) wrong"

end program struct_type_13
