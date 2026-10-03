! Procedures with implicit interfaces referenced in OpenMP parallel loops:
! through a dummy procedure with an explicit interface, in internal functions
! and a statement function called in the loop, and with an internal procedure
! passed as an actual argument.
program implicit_interface_90
    implicit none
    interface
        real function ii90_iface(x)
            real, intent(in) :: x
        end function
    end interface
    real, external :: ii90_g
    external :: ii90_apply
    real :: a(8), sf, y
    integer :: i
    sf(y) = ii90_g(y) + 1.0
    call drive(ii90_g, a, 8)
    if (abs(sum(a) - 72.0) > 1e-4) error stop 1
    a = 0
    !$omp parallel do
    do i = 1, 8
        a(i) = wrap(real(i)) + sf(real(i))
    end do
    !$omp end parallel do
    if (abs(sum(a) - 160.0) > 1e-4) error stop 2
    a = 0
    !$omp parallel do
    do i = 1, 8
        call ii90_apply(triple, real(i), a(i))
    end do
    !$omp end parallel do
    if (abs(sum(a) - 108.0) > 1e-4) error stop 3
    a = 0
    !$omp parallel do
    do i = 1, 8
        a(i) = local_external(real(i))
    end do
    !$omp end parallel do
    if (abs(sum(a) - 72.0) > 1e-4) error stop 4
    print *, sum(a)
contains
    subroutine drive(f, b, n)
        procedure(ii90_iface) :: f
        integer, intent(in) :: n
        real, intent(out) :: b(n)
        integer :: j
        !$omp parallel do
        do j = 1, n
            b(j) = f(real(j))
        end do
        !$omp end parallel do
    end subroutine

    real function wrap(x)
        real, intent(in) :: x
        wrap = ii90_g(x) + 1.0
    end function

    real function triple(x)
        real, intent(in) :: x
        triple = 3*x
    end function

    real function local_external(x)
        real, intent(in) :: x
        real, external :: ii90_g
        local_external = ii90_g(x)
    end function
end program

real function ii90_g(x)
    real, intent(in) :: x
    ii90_g = 2*x
end function

subroutine ii90_apply(f, x, y)
    real, external :: f
    real, intent(in) :: x
    real, intent(out) :: y
    y = f(x)
end subroutine
