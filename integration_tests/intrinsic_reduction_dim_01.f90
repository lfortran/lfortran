program intrinsic_reduction_dim_01
    ! COUNT, PARITY and NORM2 with DIM, in particular DIM equal to the last
    ! dimension, including a zero extent in the reduced dimension.
    implicit none
    logical :: m2(2, 3), m3(2, 3, 4), mb(0:1, -1:1)
    logical :: empty2(3, 0), empty3(2, 3, 0)
    real :: v2(2, 3), v3(2, 3, 4), vempty2(3, 0), vempty3(2, 3, 0)
    integer :: i, j, k, dim
    integer :: c2(2), c3(2, 3), cb(0:1), ce(3)
    logical :: p2(2), p3(2, 3), pe(3)
    real :: n2(2), n3(2, 3), ne(3)

    m2 = reshape([.true., .false., .true., .true., .false., .true.], [2, 3])
    v2 = reshape([3.0, 0.0, 4.0, 0.0, 0.0, 2.0], [2, 3])
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                m3(i, j, k) = mod(i + 2*j + 3*k, 3) == 0
                v3(i, j, k) = real(i + 10*j + 100*k)
            end do
        end do
    end do

    ! Rank 2, DIM = 1
    if (any(count(m2, dim=1) /= [1, 2, 1])) error stop "count r2 dim1"
    if (any(parity(m2, dim=1) .neqv. [.true., .false., .true.])) &
        error stop "parity r2 dim1"
    if (any(abs(norm2(v2, dim=1) - [3.0, 4.0, 2.0]) > 1e-6)) &
        error stop "norm2 r2 dim1"

    ! Rank 2, DIM = 2 (the last dimension)
    c2 = count(m2, dim=2)
    if (any(c2 /= [2, 2])) error stop "count r2 dim2"
    p2 = parity(m2, dim=2)
    if (any(p2)) error stop "parity r2 dim2"
    n2 = norm2(v2, dim=2)
    if (any(abs(n2 - [5.0, 2.0]) > 1e-6)) error stop "norm2 r2 dim2"

    ! Rank 2, run-time DIM = 2 for COUNT
    dim = 2
    c2 = -1
    c2 = count(m2, dim)
    if (any(c2 /= [2, 2])) error stop "count r2 run-time dim"

    ! Non-default lower bounds, DIM = 2
    mb = reshape([.true., .true., .false., .true., .true., .true.], [2, 3])
    cb = count(mb, dim=2)
    if (any(cb /= [2, 3])) error stop "count lbound dim2"

    ! Rank 3, every DIM, compared with rank-1 reductions of sections
    c3 = count(m3, dim=3)
    p3 = parity(m3, dim=3)
    n3 = norm2(v3, dim=3)
    do j = 1, 3
        do i = 1, 2
            if (c3(i, j) /= count(m3(i, j, :))) error stop "count r3 dim3"
            if (p3(i, j) .neqv. parity(m3(i, j, :))) error stop "parity r3 dim3"
            if (abs(n3(i, j) - sqrt(sum(v3(i, j, :)**2))) > 1e-3) &
                error stop "norm2 r3 dim3"
        end do
    end do
    if (any(count(m3, dim=2) /= &
            reshape([((count(m3(i, :, k)), i = 1, 2), k = 1, 4)], [2, 4]))) &
        error stop "count r3 dim2"
    if (any(count(m3, dim=1) /= &
            reshape([((count(m3(:, j, k)), j = 1, 3), k = 1, 4)], [3, 4]))) &
        error stop "count r3 dim1"
    if (any(parity(m3, dim=2) .neqv. &
            reshape([((parity(m3(i, :, k)), i = 1, 2), k = 1, 4)], [2, 4]))) &
        error stop "parity r3 dim2"
    if (any(abs(norm2(v3, dim=1) - &
            reshape([((norm2(v3(:, j, k)), j = 1, 3), k = 1, 4)], [3, 4])) > 1e-3)) &
        error stop "norm2 r3 dim1"

    ! Zero extent in the reduced (last) dimension: every result element
    ! must still be written. The results are poisoned first.
    empty2 = .false.
    vempty2 = 0.0
    ce = -1
    ce = count(empty2, dim=2)
    if (any(ce /= 0)) error stop "empty count r2"
    pe = .true.
    pe = parity(empty2, dim=2)
    if (any(pe)) error stop "empty parity r2"
    ne = -1.0
    ne = norm2(vempty2, dim=2)
    if (any(ne /= 0.0)) error stop "empty norm2 r2"

    empty3 = .false.
    vempty3 = 0.0
    c3 = -1
    c3 = count(empty3, dim=3)
    if (any(c3 /= 0)) error stop "empty count r3"
    p3 = .true.
    p3 = parity(empty3, dim=3)
    if (any(p3)) error stop "empty parity r3"
    n3 = -1.0
    n3 = norm2(vempty3, dim=3)
    if (any(n3 /= 0.0)) error stop "empty norm2 r3"

    print *, "ok"
end program
