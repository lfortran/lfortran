program derived_types_vector_subscript_01
    implicit none
    type :: cell
        integer :: n
    end type
    type(cell) :: c(6)
    complex :: z(4)
    real :: r(2)
    integer :: idx(2), got(2), i
    c%n = [(10*i, i = 1, 6)]
    if (sum(c([3, 4])%n) /= 70) error stop
    idx = [5, 2]
    got = c(idx)%n
    print *, got
    if (any(got /= [50, 20])) error stop
    z = [(cmplx(i, -i), i = 1, 4)]
    r = z([4, 1])%re
    print *, r
    if (any(r /= [4.0, 1.0])) error stop
end program
