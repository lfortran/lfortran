program count_any_all_parity_dim_fold_01
    ! count, any, all and parity with dim= of constant masks of rank 2 and 3
    ! fold along dim (#17239, #18049), in named constants and in executable
    ! statements.
    implicit none
    logical, parameter :: g(2,3) = reshape([.true., .false., .false., &
        .false., .true., .true.], [2,3])
    integer, parameter :: tbl(2,3) = reshape([4, 9, 1, 8, 2, 7], [2,3])
    real(8), parameter :: m(2,3) = reshape([1d0, -2d0, 3d0, 3d0, -5d0, 2d0], &
        [2,3])
    logical, parameter :: g3(2,3,2) = reshape([.true., .false., .true., &
        .true., .false., .false., .false., .false., .true., .false., &
        .true., .true.], [2,3,2])
    logical, parameter :: z(0,3) = reshape([logical ::], [0,3])
    integer, parameter :: d2 = 2

    integer, parameter :: c1(3) = count(g, dim=1)
    integer, parameter :: c2(2) = count(g, dim=d2)
    integer(8), parameter :: c8(3) = count(tbl > 5, dim=1, kind=8)
    integer(2), parameter :: c2k(2) = count(tbl < 3, dim=2, kind=2)
    integer, parameter :: cm(3) = count(m > 0, dim=1)
    logical, parameter :: a1(3) = any(g, dim=1)
    logical, parameter :: a2(2) = any(m > 2, dim=2)
    logical, parameter :: l1(3) = all(m > -3, dim=1)
    logical, parameter :: l2(2) = all(g, dim=2)
    logical, parameter :: p1(3) = parity(g, dim=1)
    logical, parameter :: p2(2) = parity(m > 0, dim=2)
    integer, parameter :: c3_1(3,2) = count(g3, dim=1)
    integer, parameter :: c3_2(2,2) = count(g3, dim=2)
    integer, parameter :: c3_3(2,3) = count(g3, dim=3)
    logical, parameter :: a3_2(2,2) = any(g3, dim=2)
    logical, parameter :: l3_3(2,3) = all(g3, dim=3)
    logical, parameter :: p3_1(3,2) = parity(g3, dim=1)
    integer, parameter :: cz1(3) = count(z, dim=1)
    logical, parameter :: lz1(3) = all(z, dim=1)
    logical, parameter :: az1(3) = any(z, dim=1)
    integer, parameter :: nz2 = size(count(z, dim=2))
    integer, parameter :: c_rank1 = count([.true., .false., .true.], dim=1)

    integer :: c(3), d
    integer(8) :: k8(2)
    logical :: a(3), l(2)

    if (any(c1 /= [1, 0, 2])) error stop 1
    if (any(c2 /= [2, 1])) error stop 2
    if (any(c8 /= [1_8, 1_8, 1_8])) error stop 3
    if (any(c2k /= [2_2, 0_2])) error stop 4
    if (any(cm /= [1, 2, 1])) error stop 5
    if (any(a1 .neqv. [.true., .false., .true.])) error stop 6
    if (any(a2 .neqv. [.true., .true.])) error stop 7
    if (any(l1 .neqv. [.true., .true., .false.])) error stop 8
    if (any(l2 .neqv. [.false., .false.])) error stop 9
    if (any(p1 .neqv. [.true., .false., .false.])) error stop 10
    if (any(p2 .neqv. [.false., .false.])) error stop 11
    if (any(c3_1 /= reshape([1, 2, 0, 0, 1, 2], [3,2]))) error stop 12
    if (any(c3_2 /= reshape([2, 1, 2, 1], [2,2]))) error stop 13
    if (any(c3_3 /= reshape([1, 0, 2, 1, 1, 1], [2,3]))) error stop 14
    if (any(a3_2 .neqv. reshape([.true., .true., .true., .true.], [2,2]))) &
        error stop 15
    if (any(l3_3 .neqv. reshape([.false., .false., .true., .false., &
        .false., .false.], [2,3]))) error stop 16
    if (any(p3_1 .neqv. reshape([.true., .false., .false., .false., .true., &
        .false.], [3,2]))) error stop 17
    if (any(cz1 /= 0)) error stop 18
    if (.not. all(lz1)) error stop 19
    if (any(az1)) error stop 20
    if (nz2 /= 0) error stop 21
    if (c_rank1 /= 2) error stop 22

    ! The same reductions in executable statements
    c = count(g, dim=1)
    if (any(c /= [1, 0, 2])) error stop 23
    c = count(m > 0, dim=1)
    if (any(c /= [1, 2, 1])) error stop 24
    k8 = count(tbl < 3, dim=2, kind=8)
    if (any(k8 /= [2_8, 0_8])) error stop 25
    a = any(m > 2, dim=1)
    if (any(a .neqv. [.false., .true., .false.])) error stop 26
    l = all(m > -3, dim=2)
    if (any(l .neqv. [.false., .true.])) error stop 27
    a = parity(m > 0, dim=1)
    if (any(a .neqv. [.true., .false., .true.])) error stop 28
    if (any(count(g3, dim=3) /= reshape([1, 0, 2, 1, 1, 1], [2,3]))) &
        error stop 29
    if (size(count(z, dim=2)) /= 0) error stop 30
    ! dim that is not a constant
    d = 2
    if (any(count(g, dim=d) /= [2, 1])) error stop 31
    if (any(parity(g, dim=d) .neqv. [.false., .true.])) error stop 32
    print *, c1, c2, c8, c2k, a1, l2, p1
end program
