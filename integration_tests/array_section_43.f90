program array_section_43
    ! Sections with an omitted bound of arrays whose bounds lie beyond
    ! huge(0), compiled with --descriptor-index-64. The omitted bound is
    ! the array's own lbound or ubound, which has to be taken in
    ! integer(8): in integer(4) it is truncated, while the explicit bound
    ! beside it is exact (#15067).
    implicit none
    type :: t
        real, allocatable :: a(:)
    end type
    type(t) :: s
    type(t), allocatable :: u(:)
    real, allocatable :: b(:), z(:,:)
    integer(8) :: k8, lo8, i8
    integer :: j, n4
    k8 = 2_8**32 + 1
    allocate(b(k8:k8 + 7))
    b = 1
    if (size(b(k8:), kind=8) /= 8) error stop 1
    if (size(b(k8 + 5:), kind=8) /= 3) error stop 2
    if (size(b(:k8 + 2), kind=8) /= 3) error stop 3
    if (abs(sum(b(k8:)) - 8) > 1e-4) error stop 4
    lo8 = 3000000000_8
    n4 = 3
    allocate(z(lo8:lo8 + 6, 2))
    do j = 1, 2
        do i8 = lo8, lo8 + 6
            z(i8, j) = real(i8 - lo8 + 1)*j
        end do
    end do
    print *, sum(z(lo8 + n4:, 1)), sum(z(:lo8 + n4, 2))
    if (abs(sum(z(lo8 + n4:, 1)) - 22) > 1e-4) error stop 5
    if (abs(sum(z(:lo8 + n4, 2)) - 20) > 1e-4) error stop 6
    if (abs(sum(z(lo8 + 1:lo8 + 5:2, 2)) - 24) > 1e-4) error stop 7
    z(:lo8 + 1, 1) = 0
    if (abs(sum(z(:, 1)) - 25) > 1e-4) error stop 8
    z(lo8 + 5:, 2) = 0
    if (abs(sum(z(:, 2)) - 30) > 1e-4) error stop 9
    print *, sum(z(:, 1)), sum(z(:, 2))
    allocate(s%a(lo8:lo8 + 6), u(2))
    allocate(u(2)%a(lo8:lo8 + 6))
    do i8 = lo8, lo8 + 6
        s%a(i8) = real(i8 - lo8 + 1)
        u(2)%a(i8) = real(i8 - lo8 + 1)
    end do
    if (abs(sum(s%a(lo8 + n4:)) - 22) > 1e-4) error stop 10
    if (abs(sum(s%a(:lo8 + n4)) - 10) > 1e-4) error stop 11
    if (abs(sum(u(2)%a(lo8 + n4:)) - 22) > 1e-4) error stop 12
    if (abs(sum(u(2)%a(:lo8 + n4)) - 10) > 1e-4) error stop 13
end program
