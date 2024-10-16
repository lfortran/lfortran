program intrinsics_233
    implicit none
    real :: x
    real :: z = -1.0
    real :: y(3)
    x = -1.0
    x = sqrt(x)
    y = [sqrt(x), log(x), exp(x)]

    print*, x
    print *, isnan(x)
    if (isnan(x) .neqv. .true.) error stop
    print*, y(1)
    print *, isnan(y(1))
    if (isnan(y(1)) .neqv. .true.) error stop
    print*, y(2)
    print *, isnan(y(2))
    if (isnan(y(2)) .neqv. .true.) error stop
    print*, y(3)
    print *, isnan(y(3))
    if (isnan(y(3)) .neqv. .true.) error stop

    !tests for compile time argument
    print *, isnan(1.0)
    if (isnan(1.0) .neqv. .false.) error stop
    print *, isnan(sqrt(z))
    if (isnan(sqrt(z)) .neqv. .true.) error stop

end program