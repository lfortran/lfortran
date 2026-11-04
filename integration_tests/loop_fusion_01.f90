program loop_fusion_01
    ! Adjacent loop nests over the same iteration space, some of which may
    ! be fused into one nest (with --fast) and some of which may not.
    implicit none
    integer, parameter :: n = 6
    real(8) :: x(0:n+1,0:n+1), xnew(0:n+1,0:n+1), delta, s
    real(8) :: a(n), b(n), c(n), d(n), m(n,n), p(n,n), q(n,n)
    real(8), allocatable :: ya(:,:), yanew(:,:)
    integer :: i, j, k, it

    ! A reduction and a copy of the same sections (fused).
    x = 0
    xnew = 0
    x(n+1,1:n) = 1
    do it = 1, 3
        do j = 1, n
            do i = 1, n
                xnew(i,j) = (x(i-1,j) + x(i+1,j) + x(i,j+1) + x(i,j-1)) / 4
            end do
        end do
        delta = maxval(abs(xnew(1:n,1:n) - x(1:n,1:n)))
        x(1:n,1:n) = xnew(1:n,1:n)
    end do
    if (abs(delta - 0.078125_8) > 1e-12_8) error stop 1
    if (abs(sum(x) - 9.28125_8) > 1e-12_8) error stop 2

    ! The same with allocatable arrays (fused).
    allocate(ya(0:n+1,0:n+1), yanew(0:n+1,0:n+1))
    ya = 0
    yanew = 0
    ya(n+1,1:n) = 1
    do it = 1, 3
        do j = 1, n
            do i = 1, n
                yanew(i,j) = (ya(i-1,j) + ya(i+1,j) + ya(i,j+1) + ya(i,j-1)) / 4
            end do
        end do
        delta = maxval(abs(yanew(1:n,1:n) - ya(1:n,1:n)))
        ya(1:n,1:n) = yanew(1:n,1:n)
    end do
    if (abs(delta - 0.078125_8) > 1e-12_8) error stop 14
    if (abs(sum(ya) - 9.28125_8) > 1e-12_8) error stop 15

    ! The second loop reads an element the first one writes in a later
    ! iteration (not fused).
    a = [(real(i, 8), i = 1, n)]
    b = 0
    c(1:n-1) = a(2:n)
    a(1:n-1) = c(1:n-1) * 2
    b(1:n-1) = a(2:n)
    if (any(b(1:n-1) /= [6, 8, 10, 12, 6])) error stop 3

    ! A reduction whose result the second loop uses (not fused).
    a = [(real(i, 8), i = 1, n)]
    s = sum(a(1:n))
    a(1:n) = a(1:n) / s
    if (abs(sum(a) - 1) > 1e-12_8) error stop 4

    ! The same element of `d` in every iteration of the outer loop (not
    ! fused).
    d = 0
    do j = 1, n
        do i = 1, n
            d(i) = d(i) + j
        end do
    end do
    do j = 1, n
        do i = 1, n
            m(i,j) = d(i)
        end do
    end do
    if (any(m /= 21)) error stop 5

    ! The same loop variable (fused); its value after the loops.
    do i = 1, n
        a(i) = i
    end do
    do i = 1, n
        b(i) = a(i) * 2
    end do
    if (i /= n + 1) error stop 6
    if (any(b /= [2, 4, 6, 8, 10, 12])) error stop 7

    ! A loop variable used after the loops (not renamed, not fused).
    do i = 1, n
        a(i) = -i
    end do
    do k = 1, n
        b(k) = a(k) + 1
    end do
    if (k /= n + 1) error stop 8
    if (any(b /= [0, -1, -2, -3, -4, -5])) error stop 9

    ! Sections with other lower bounds (fused).
    do j = 1, n
        do i = 1, n
            m(i,j) = 10*i + j
        end do
    end do
    p = 0
    q = 0
    p(1:n-2,2:n) = m(3:n,1:n-1) * 2
    q(2:n-1,1:n-1) = p(1:n-2,2:n) + m(3:n,1:n-1)
    if (any(p(1:n-2,2:n) /= 2*m(3:n,1:n-1))) error stop 10
    if (any(q(2:n-1,1:n-1) /= 3*m(3:n,1:n-1))) error stop 11
    if (sum(p) /= 2*sum(m(3:n,1:n-1)) .or. sum(q) /= 3*sum(m(3:n,1:n-1))) error stop 12
    ! Aligned, the second loop reads an element the first one writes in a
    ! later iteration (not fused).
    p = 0
    p(2:n-1,1:n) = m(1:n-2,1:n) + 1
    q(1:n-2,1:n) = p(3:n,1:n)
    if (any(q(1:n-3,:) /= m(2:n-2,:) + 1) .or. any(q(n-2,:) /= 0)) error stop 13
    print *, "ok"
end program
