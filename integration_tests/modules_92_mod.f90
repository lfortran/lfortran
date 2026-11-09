module modules_92_sizes
    implicit none
    type :: dims_t
        integer :: n = 3
        integer :: m = 2
        integer :: k = 4
    end type
    type(dims_t) :: dims
end module

module modules_92_sizes2
    implicit none
    type :: cnt_t
        integer :: ng = 5
    end type
    type(cnt_t) :: cnt
end module

! An external procedure defined before the module that calls it.
subroutine modules_92_fill_before(p)
    use modules_92_sizes, only: dims
    implicit none
    real :: p(dims%n)
    p = 1.0
end subroutine

module modules_92_m
    implicit none
contains
    subroutine run_all()
        use modules_92_sizes, only: dims
        real :: x(3), y(2, 8), z(5)
        complex :: c(5, 2, 3)
        call modules_92_fill_before(x)
        if (any(x /= 1.0)) error stop
        call modules_92_fill2(y)
        if (any(y /= 2.0)) error stop
        call modules_92_fill3(z)
        if (any(z /= 3.0)) error stop
        c = (0.0, 0.0)
        call modules_92_fill4(c, 2)
        if (any(c(:, :, 1) /= (4.0, 1.0))) error stop
        if (any(c(:, :, 2:) /= (0.0, 0.0))) error stop
        if (dims%n /= 3) error stop
    end subroutine
end module

! External procedures defined after the module that calls them.
subroutine modules_92_fill2(p)
    use modules_92_sizes, only: dims
    implicit none
    real :: p(dims%m, dims%m*dims%k)
    if (size(p, 1) /= 2) error stop
    if (size(p, 2) /= 8) error stop
    p = 2.0
end subroutine

subroutine modules_92_fill3(p)
    use modules_92_sizes2, only: cnt
    implicit none
    real :: p(cnt%ng)
    if (size(p) /= 5) error stop
    p = 3.0
end subroutine

subroutine modules_92_fill4(c0, nstate)
    use modules_92_sizes2, only: cnt
    implicit none
    integer :: nstate
    complex :: c0(cnt%ng, nstate, *)
    if (size(c0, 1) /= 5) error stop
    if (size(c0, 2) /= 2) error stop
    c0(:, :, 1) = (4.0, 1.0)
end subroutine
