program parameter_15
    implicit none
    integer i1
    real r1
    integer(8) i2
    real(8) r2
    parameter (i1=1_8)
    parameter (r1=1.0D+0)
    parameter (i2=1)
    parameter (r2=1.0)
    print *, i1, kind(i1)
    if (i1 /= 1) error stop
    if (kind(i1) /= 4) error stop
    print *, r1, kind(r1)
    if ((r1 - 1.0) > 1e-6) error stop
    if (kind(r1) /= 4) error stop
    print *, i2, kind(i2)
    if (i2 /= 1) error stop
    if (kind(i2) /= 8) error stop
    print *, r2, kind(r2)
    if ((r2 - 1.0) > 1e-6) error stop
    if (kind(r2) /= 8) error stop
end program