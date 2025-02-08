program intrinsics_1arg
    implicit none
    
    real :: x1
    integer :: i1
    logical :: l1
    character(len=10) :: str
    real, parameter :: eps = 1e-8

    ! Mathematical functions
    x1 = abs(a = 4.0)
    print *, x1
    if (abs(x1 - 4.0) > eps) error stop

    x1 = acos(x = 0.5)
    print *, x1
    if (abs(x1 - 1.04719755) > eps) error stop

    x1 = asin(x = 0.5)
    print *, x1
    if (abs(x1 - 0.52359878) > eps) error stop

    x1 = atan(x = 1.0)
    print *, x1
    if (abs(x1 - 0.78539816) > eps) error stop

    x1 = cos(x = 0.0)
    print *, x1
    if (abs(x1 - 1.0) > eps) error stop

    x1 = cosh(x = 0.0)
    print *, x1
    if (abs(x1 - 1.0) > eps) error stop

    x1 = erf(x = 1.0)
    print *, x1

    x1 = erfc(x = 1.0)
    print *, x1

    x1 = exp(x = 1.0)
    print *, x1

    x1 = gamma(x =0.5)
    print *, x1

    x1 = log(x = 2.0)
    print *, x1

    x1 = log10(x = 10.0)
    print *, x1

    x1 = sin(x = 1.0)
    print *, x1

    x1 = sinh(x = 1.0)
    print *, x1

    x1 = sqrt(x = 4.0)
    print *, x1

    x1 = tan(x = 1.0)
    print *, x1

    x1 = tanh(x = 1.0)
    print *, x1

    ! Integer functions
    i1 = iabs(a = -5)
    print *, i1
    if (i1 /= 5) error stop

    i1 = not(i = 0)
    print *, i1

    ! String functions
    str = adjustl(str = "  text")
    print *, "'" // str // "'"

    str = adjustr(str = "text  ")
    print *, "'" // str // "'"

    i1 = len(str = "hello")
    print *, i1

    str = trim(str = "hello   ")
    print *, "'" // str // "'"

    ! Kind functions
    i1 = kind(x = 1.0)
    print *, i1

    i1 = precision(x = 1.0)
    print *, i1

    i1 = range(x = 1.0)
    print *, i1

    x1 = tiny(x = 1.0)
    print *, x1

    x1 = huge(x = 1.0)
    print *, x1

    x1 = epsilon(x = 1.0)
    print *, x1

    i1 = digits(x = 1.0)
    print *, i1

    i1 = radix(x = 1.0)
    print *, i1

    ! Array functions
    integer, allocatable :: arr(:)
    allocate(arr(5))
    print *, allocated(x = arr)
    print *, shape(x = arr)
    print *, size(x = arr)
    deallocate(arr)

    ! Bit manipulation
    i1 = leadz( i = 8)
    print *, i1

    i1 = trailz(i = 8)
    print *, i1
    
end program intrinsics_1arg