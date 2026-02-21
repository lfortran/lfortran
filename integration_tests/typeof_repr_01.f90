program typeof_repr_01
    implicit none

    real(8) :: x
    integer :: a(3)
    character(:), allocatable :: s
    type(type_info) :: t

    x = 3.0_8
    a = [1, 2, 3]

    t = typeof(x)
    if (repr(t) /= "real(8)") error stop 1

    s = repr(x)
    if (index(s, "real(8) :: x") /= 1) error stop 2

    s = repr(a)
    if (index(s, "integer(4)") == 0) error stop 3

    print *, typeof(x)
    print *, repr(x)
end program typeof_repr_01
