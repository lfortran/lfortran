program implicit_interface_113
! A complex function passed to a `complex, external` dummy (#16574)
implicit none
complex :: z
complex(8) :: w
z = apply(cf)
print *, z
if (abs(z - (1.0, -1.0)) > 1e-6) error stop
z = apply_x(cfx, 2.0)
print *, z
if (abs(z - (2.0, -2.0)) > 1e-6) error stop
w = apply8(cf8)
print *, w
if (abs(w - (1.5d0, -2.5d0)) > 1d-12) error stop
contains
complex function apply(f)
    complex, external :: f
    apply = f()
end function
complex function apply_x(f, x)
    complex, external :: f
    real :: x
    apply_x = f(x)
end function
complex(8) function apply8(f)
    complex(8), external :: f
    apply8 = f()
end function
complex function cf()
    cf = (1.0, -1.0)
end function
complex function cfx(x)
    real :: x
    cfx = cmplx(x, -x)
end function
complex(8) function cf8()
    cf8 = (1.5d0, -2.5d0)
end function
end program
