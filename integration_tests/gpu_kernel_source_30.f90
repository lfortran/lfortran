module gpu_kernel_source_30_mod
    implicit none
    type :: fields_t
        real, allocatable :: q(:), r(:)
    end type
    real :: c1(8), c2(8)
    common /gpu_kernel_source_30_blk/ c1, c2
contains
    ! Distinct dummy arguments: a kernel buffer of each is not aliased.
    subroutine axpy(n, alpha, x, y)
        integer, intent(in) :: n
        real, intent(in) :: alpha, x(n)
        real, intent(inout) :: y(n)
        integer :: i
        do concurrent (i = 1:n)
            y(i) = y(i) + alpha * x(i)
        end do
    end subroutine
end module

program gpu_kernel_source_30
    use gpu_kernel_source_30_mod
    implicit none
    integer, parameter :: n = 8
    real, allocatable :: a(:), b(:), s(:)
    real, target :: t(2*n)
    real, pointer :: p(:)
    real :: e1(n), e2(n)
    equivalence (e1, e2)
    type(fields_t) :: f
    integer :: i

    allocate(a(n), b(n), s(2*n), f%q(n), f%r(n))

    ! Different arrays: every buffer is not aliased.
    do concurrent (i = 1:n)
        a(i) = i
        b(i) = 2*i
    end do
    call axpy(n, 3.0, b, a)
    do i = 1, n
        if (abs(a(i) - 7*i) > 1e-6) error stop 1
    end do

    ! A pointer and the target it points to: neither is marked.
    t = [(real(i), i = 1, 2*n)]
    p => t(n+1:2*n)
    do concurrent (i = 1:n)
        t(i) = p(i) + a(i)
    end do
    do i = 1, n
        if (abs(t(i) - (n + i + 7*i)) > 1e-6) error stop 2
    end do

    ! An associate name for a section of the array the loop writes.
    s = [(real(i), i = 1, 2*n)]
    associate (upper => s(n+1:2*n))
        do concurrent (i = 1:n)
            s(i) = upper(i) * 2
        end do
    end associate
    do i = 1, n
        if (abs(s(i) - 2*(n + i)) > 1e-6) error stop 3
    end do

    ! Different components of one derived-type variable never overlap.
    do concurrent (i = 1:n)
        f%q(i) = 10*i
    end do
    do concurrent (i = 1:n)
        f%r(i) = f%q(i) + b(i)
    end do
    do i = 1, n
        if (abs(f%r(i) - 12*i) > 1e-6) error stop 4
    end do

    ! Equivalenced and common block storage: not marked.
    e1 = 1
    do concurrent (i = 1:n)
        e1(i) = e2(i) + i
    end do
    do i = 1, n
        if (abs(e2(i) - (1 + i)) > 1e-6) error stop 5
    end do
    c2 = 5
    do concurrent (i = 1:n)
        c1(i) = c2(i) * i
    end do
    do i = 1, n
        if (abs(c1(i) - 5*i) > 1e-6) error stop 6
    end do

    print *, sum(a), sum(t(1:n)), sum(s(1:n)), sum(f%r), sum(e1), sum(c1)
end program
