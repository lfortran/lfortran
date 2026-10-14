! Regression test: an entity named in a DIMENSION statement before its type
! declaration belongs to the scope of that statement. A later scope that
! declared an entity of the same name took over the earlier array: a scalar
! of another type turned it into an array of that type and failed ASR
! verification, and a later DIMENSION statement was a duplicate.
module attr_dim_06_m
contains
    real function first(x)
        dimension :: x(2)
        real, intent(in) :: x
        first = x(1) + x(2)
    end function
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
    integer function last(x)
        dimension :: x(3)
        integer, intent(in) :: x
        last = x(3)
    end function
end module

program attr_dim_06
    use attr_dim_06_m
    implicit none
    if (abs(first([1.5, 2.5]) - 4.0) > 1e-6) error stop 1
    if (twice(3) /= 6) error stop 2
    if (last([4, 5, 6]) /= 6) error stop 3
    print '(a)', 'attr_dim_06 ok'
end program
