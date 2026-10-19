! A name dimensioned by a DIMENSION statement in one program unit must not
! affect a dummy of the same name declared in a later program unit.
subroutine attr_dim_07_fill(x)
    dimension x(*)
    real :: x
    x(1) = 1.5
    x(2) = 2.5
end subroutine attr_dim_07_fill

subroutine attr_dim_07_scalar(x)
    integer :: x
    x = 5
end subroutine attr_dim_07_scalar

subroutine attr_dim_07_shape(x, s)
    integer, intent(in) :: x(:)
    integer, intent(out) :: s
    s = sum(x)
end subroutine attr_dim_07_shape

program attr_dim_07
    interface
        subroutine attr_dim_07_shape(x, s)
            integer, intent(in) :: x(:)
            integer, intent(out) :: s
        end subroutine attr_dim_07_shape
    end interface
    real :: a(2)
    integer :: k, s
    call attr_dim_07_fill(a)
    if (abs(a(1) - 1.5) > 1e-6 .or. abs(a(2) - 2.5) > 1e-6) error stop
    call attr_dim_07_scalar(k)
    if (k /= 5) error stop
    call attr_dim_07_shape([1, 2, 3], s)
    if (s /= 6) error stop
    print *, a, k, s
end program attr_dim_07
