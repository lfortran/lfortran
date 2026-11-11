! complex(16) has the 16-byte alignment of its real(16) parts, so in a
! bind(c) derived type it is laid out as in C: after a c_int it starts at
! offset 16, not 4.
module complex128_bindc_01_m
    use iso_c_binding, only: c_int
    implicit none
    type, bind(c) :: t
        integer(c_int) :: i
        complex(16) :: z
    end type
end module

program complex128_bindc_01
    use iso_c_binding, only: c_loc, c_intptr_t, c_sizeof
    use complex128_bindc_01_m, only: t
    implicit none
    type(t), target :: x, y(2)
    complex(16) :: w
    integer(c_intptr_t) :: a, b
    x%i = 7
    x%z = (1.5_16, -2.5_16)
    a = transfer(c_loc(x), a)
    b = transfer(c_loc(x%z), b)
    if (b - a /= 16) error stop 1
    w = x%z * x%z
    if (real(w, 16) /= -4.0_16) error stop 2
    if (aimag(w) /= -7.5_16) error stop 3
    if (x%i /= 7) error stop 4
    ! The size the compiler reports matches the layout it generates.
    a = transfer(c_loc(y(1)), a)
    b = transfer(c_loc(y(2)), b)
    if (b - a /= 32 + 16) error stop 5
    if (c_sizeof(x) /= 32 + 16) error stop 6
    if (storage_size(x) /= 8 * (32 + 16)) error stop 7
    print *, "ok"
end program
