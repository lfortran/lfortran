! An offloaded `do concurrent` inlines a callee that needs two run-time
! sized per-thread temporaries: `z_scratch`, allocated from a name the
! ASSOCIATE construct binds, and `m_out`, whose own extent is `size` of
! the first one.
!
! Both become per-thread slices of a workspace buffer, and each slice's
! base pointer is computed on entry to the scope -- ahead of the
! statements that give the scope's own names their values. The extent of
! `m_out` used to be rendered through the ASSOCIATE name itself, so the
! stride from one thread's slice to the next was read out of a variable
! that had not been assigned yet: thread 0 happened to be right and every
! other thread got an aliasing or out-of-range slice.
program gpu_metal_312
implicit none
integer :: m, n, i, j
real, allocatable :: d(:), u(:, :)

m = 6
n = 5
allocate(d(n), u(m, n))

do j = 1, n
    do i = 1, m
        u(i, j) = real(10 * j + i)
    end do
end do

d = -1.0

do concurrent (j = 1:n)
    d(j) = weighted(u(:, j))
end do

do j = 1, n
    if (abs(d(j) - expected(j)) > 1.0e-3) error stop
end do

print *, "PASS"

contains

    pure function expected(col) result(r)
        integer, intent(in) :: col
        real :: r
        integer :: k
        r = 0.0
        do k = 1, m
            r = r + 2.0 * real(10 * col + k) * real(k + 1)
        end do
    end function

    pure function weighted(v) result(r)
        real, intent(in) :: v(:)
        real :: r
        real, allocatable :: m_out(:)
        real, allocatable :: z_scratch(:)
        integer :: k
        associate (blk => size(v))
            allocate(z_scratch(blk))
            do k = 1, blk
                z_scratch(k) = 2.0 * v(k)
            end do
            allocate(m_out(size(z_scratch) + 1))
            m_out(1) = 0.0
            do k = 1, size(z_scratch)
                m_out(k + 1) = z_scratch(k)
            end do
            r = 0.0
            do k = 1, size(m_out)
                r = r + m_out(k) * real(k)
            end do
        end associate
    end function

end program
