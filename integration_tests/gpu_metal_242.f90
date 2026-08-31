! A `do concurrent` whose body opens a BLOCK with a run-time sized local
! array. The Metal Shading Language has no variable-length arrays and no
! heap, so such a local is bound to a per-thread slice of a device
! workspace buffer. The workspace scan used to recognise only a bare
! `Array` local, so an `allocatable` one -- whose extents live on the
! ALLOCATE, not on the type -- got no buffer and the loop was declined.
!
! Covered here: an allocatable local sized from a kernel argument, an
! automatic local, a rank-2 allocatable local, several such locals in one
! block (their workspace slices must not collide with each other or
! between threads) and, as a fence, a fixed-size local that needs no
! workspace at all.
program gpu_metal_242
implicit none
integer, parameter :: n = 5, m = 4
real :: a(m, n), b(m, n), c(m, n), d(m, n)
real :: e(m, m, n)
integer :: i, j, k

a = 0.0
b = 0.0
c = 0.0
d = 0.0
e = 0.0

call run_kernel(a, b, c, d, e, n, m)

do j = 1, n
    do i = 1, m
        if (abs(a(i, j) - real(100*j + i)) > 1.0e-4) error stop "a"
        if (abs(b(i, j) - real(200*j + i)) > 1.0e-4) error stop "b"
        if (abs(c(i, j) - real(300*j + i)) > 1.0e-4) error stop "c"
        if (abs(d(i, j) - real(7*j + i)) > 1.0e-4) error stop "d"
        do k = 1, m
            if (abs(e(k, i, j) - real(1000*j + 10*i + k)) > 1.0e-4) then
                error stop "e"
            end if
        end do
    end do
end do

print *, "ok"

contains

    subroutine run_kernel(a, b, c, d, e, nn, mm)
    real, intent(out) :: a(:,:), b(:,:), c(:,:), d(:,:)
    real, intent(out) :: e(:,:,:)
    integer, intent(in) :: nn, mm
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            ! Extents come from the kernel argument `mm`.
            real, allocatable :: t(:)
            real, allocatable :: r2(:,:)
            ! An automatic array: the extent is on the type itself.
            real :: u(mm)
            ! Fence: a fixed-size local needs no workspace.
            real :: f(4)
            integer :: p, q
            allocate(t(mm))
            allocate(r2(mm, mm))
            do p = 1, mm
                t(p) = real(100*jj + p)
                u(p) = real(200*jj + p)
                f(p) = real(7*jj + p)
                do q = 1, mm
                    r2(q, p) = real(1000*jj + 10*p + q)
                end do
            end do
            a(:, jj) = t
            b(:, jj) = u
            c(:, jj) = t + real(200*jj)
            d(:, jj) = f
            do p = 1, mm
                do q = 1, mm
                    e(q, p, jj) = r2(q, p)
                end do
            end do
        end block
    end do
    end subroutine

end program
