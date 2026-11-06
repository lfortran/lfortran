module gpu_metal_600_m
    implicit none
    type :: t
        real, allocatable :: a(:)
        integer :: n
    end type
contains
    pure real function get_s(s, i) result(r)
        type(t), intent(in) :: s
        integer(8), intent(in) :: i
        r = s%a(i) + s%n
    end function

    pure real function get_a(a, i) result(r)
        real, allocatable, intent(in) :: a(:)
        integer(8), intent(in) :: i
        r = a(i) + 7
    end function
end module

program gpu_metal_600
    ! Offloaded loops compiled with --descriptor-index-64, so the kernels
    ! compute element offsets in 64 bits and the launch passes the extents
    ! and lower bounds of run-time-shaped arrays to them as integer(8). The
    ! arrays are small, so a run on the device (or on cuda_cpu) executes
    ! that 64-bit path: a rank-2 and a rank-3 allocatable with run-time
    ! extents and lower bounds other than 1, an integer(8) subscript, and a
    ! non-contiguous section passed to an assumed-shape dummy.
    !
    ! Arrays whose lower bound is beyond huge(0) (3000000000, or
    ! -3000000000) reach the kernel through every channel that hands it a
    ! lower bound: a component of a derived-type variable, the same passed
    ! to a device routine, an allocatable dummy of one, and a component of
    ! an array of a derived type, also passed to both. Each lower bound has
    ! to reach the integer(8) subscript it is subtracted from unwrapped,
    ! and lbound(x, 1, kind=8) in a kernel has to be the exact bound.
    ! gpu_metal_601 covers the sections and sum() a kernel lowers itself.
    use gpu_metal_600_m, only: t, get_s, get_a
    implicit none
    integer, allocatable :: a(:,:), c(:,:,:)
    real, allocatable :: x(:), y(:), w(:,:)
    type(t) :: s
    type(t), allocatable :: u(:)
    integer :: i, j, k, n, m, p, lo
    integer(8) :: i8, lo8, n8, e, cb(7)
    n = 5
    m = 4
    p = 3
    lo = -2
    allocate(a(lo:lo + n - 1, 0:m - 1))
    a = 0
    do concurrent (j = 0:m - 1, i = lo:lo + n - 1)
        a(i, j) = 100*i + j
    end do
    print *, a(lo, 0), a(lo + n - 1, m - 1)
    do j = 0, m - 1
        do i = lo, lo + n - 1
            if (a(i, j) /= 100*i + j) error stop 1
        end do
    end do

    allocate(c(2:n + 1, -1:m - 2, 3:p + 2))
    c = 0
    do concurrent (k = 3:p + 2, j = -1:m - 2, i = 2:n + 1)
        c(i, j, k) = 10000*k + 100*j + i + a(i - 4, j + 1)
    end do
    print *, c(2, -1, 3), c(n + 1, m - 2, p + 2)
    do k = 3, p + 2
        do j = -1, m - 2
            do i = 2, n + 1
                if (c(i, j, k) /= 10000*k + 100*j + i + a(i - 4, j + 1)) &
                    error stop 2
            end do
        end do
    end do

    lo8 = 3000000000_8
    n8 = 7
    allocate(x(lo8:lo8 + n8 - 1))
    do concurrent (i8 = lo8:lo8 + n8 - 1)
        x(i8) = real(i8 - lo8)
    end do
    print *, x(lo8), x(lo8 + n8 - 1)
    do i8 = lo8, lo8 + n8 - 1
        if (x(i8) /= real(i8 - lo8)) error stop 3
    end do
    do concurrent (i8 = 1:n8)
        cb(i8) = lbound(x, 1, kind=8) + ubound(x, 1, kind=8) + i8
    end do
    print *, cb(1), cb(n8)
    do i8 = 1, n8
        if (cb(i8) /= 2*lo8 + n8 - 1 + i8) error stop 12
    end do

    ! On the host these arrays are set and checked element by element.
    allocate(y(lo8:lo8 + n8 - 1), s%a(lo8:lo8 + n8 - 1))
    do i8 = lo8, lo8 + n8 - 1
        s%a(i8) = x(i8)
    end do
    s%n = 7
    do concurrent (i8 = lo8:lo8 + n8 - 1)
        y(i8) = s%a(i8) + s%n
    end do
    print *, y(lo8), y(lo8 + n8 - 1)
    do i8 = lo8, lo8 + n8 - 1
        if (y(i8) /= real(i8 - lo8) + 7) error stop 6
    end do
    do concurrent (i8 = lo8:lo8 + n8 - 1)
        y(i8) = get_s(s, i8)
    end do
    do i8 = lo8, lo8 + n8 - 1
        if (y(i8) /= real(i8 - lo8) + 7) error stop 7
    end do
    do concurrent (i8 = lo8:lo8 + n8 - 1)
        y(i8) = get_a(x, i8)
    end do
    do i8 = lo8, lo8 + n8 - 1
        if (y(i8) /= real(i8 - lo8) + 7) error stop 8
    end do

    allocate(u(2), w(2, lo8:lo8 + n8 - 1))
    do e = 1, 2
        allocate(u(e)%a(lo8:lo8 + n8 - 1))
        do i8 = lo8, lo8 + n8 - 1
            u(e)%a(i8) = real(i8 - lo8 + 100*e)
        end do
        u(e)%n = 0
    end do
    do concurrent (i8 = lo8:lo8 + n8 - 1, e = 1:2)
        w(e, i8) = u(e)%a(i8)
    end do
    print *, w(1, lo8), w(2, lo8 + n8 - 1)
    do e = 1, 2
        do i8 = lo8, lo8 + n8 - 1
            if (w(e, i8) /= real(i8 - lo8 + 100*e)) error stop 9
        end do
    end do
    do concurrent (i8 = lo8:lo8 + n8 - 1)
        w(1, i8) = get_s(u(1), i8)
        w(2, i8) = get_a(u(2)%a, i8)
    end do
    do i8 = lo8, lo8 + n8 - 1
        if (w(1, i8) /= real(i8 - lo8) + 100) error stop 10
        if (w(2, i8) /= real(i8 - lo8) + 207) error stop 11
    end do

    deallocate(s%a)
    allocate(s%a(-lo8:-lo8 + n8 - 1))
    do i8 = -lo8, -lo8 + n8 - 1
        s%a(i8) = x(i8 + 2*lo8)
    end do
    do concurrent (i8 = -lo8:-lo8 + n8 - 1)
        x(i8 + 2*lo8) = s%a(i8) + s%n
    end do
    print *, x(lo8), x(lo8 + n8 - 1)
    do i8 = lo8, lo8 + n8 - 1
        if (x(i8) /= real(i8 - lo8) + 7) error stop 16
    end do

    call twice(a(lo + 1:lo + n - 1:2, 1:m - 1))
    print *, a(lo + 1, 1), a(lo + 3, m - 1)
    do j = 0, m - 1
        do i = lo, lo + n - 1
            if (j >= 1 .and. (i == lo + 1 .or. i == lo + 3)) then
                if (a(i, j) /= 2*(100*i + j)) error stop 4
            else
                if (a(i, j) /= 100*i + j) error stop 5
            end if
        end do
    end do
contains
    subroutine twice(t)
        integer, intent(inout) :: t(:,:)
        integer :: i, j
        do concurrent (j = 1:size(t, 2), i = 1:size(t, 1))
            t(i, j) = 2*t(i, j)
        end do
    end subroutine
end program
