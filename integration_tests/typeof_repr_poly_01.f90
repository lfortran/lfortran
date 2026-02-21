program typeof_repr_poly_01
    implicit none
    class(*), allocatable :: u
    character(:), allocatable :: s

    allocate(u, source=42)

    s = typeof(u)
    if (index(s, "integer(4)") /= 1) error stop 1

    s = repr(u)
    if (index(s, "integer(4) :: u =") /= 1) error stop 2

    print *, typeof(u)
    print *, repr(u)
end program typeof_repr_poly_01
