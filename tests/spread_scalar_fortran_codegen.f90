program spread_scalar_fortran_codegen
    ! `spread` of a scalar is printed with the scalar as `source`, for a
    ! variable, a constant, and every scalar type `spread` accepts.
    implicit none
    integer :: a, r(3)
    real :: x
    logical :: l
    character(len=2) :: c
    a = 5
    x = 1.5
    l = .true.
    c = "ab"
    r = spread(a, 1, 3)
    print *, r
    print *, spread(7, 1, 2)
    print *, spread(x, 1, 2)
    print *, spread(l, 1, 2)
    print *, spread(c, 1, 2)
    print *, spread("cd", 1, 2)
end program
