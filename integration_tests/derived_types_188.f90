module derived_types_188_m
    implicit none

    type :: item
        integer, allocatable :: v(:)
    end type item

contains

    function mkarr() result(r)
        type(item) :: r(3)
        integer :: i
        do i = 1, 3
            allocate(r(i)%v(i))
            r(i)%v = i
        end do
    end function mkarr

    function mkarr_n(n) result(r)
        integer, intent(in) :: n
        type(item) :: r(n)
        integer :: i
        do i = 1, n
            allocate(r(i)%v(i))
            r(i)%v = i
        end do
    end function mkarr_n

    function mkarr_2d(n) result(r)
        integer, intent(in) :: n
        type(item) :: r(n, 2)
        integer :: i, j
        do j = 1, 2
            do i = 1, n
                allocate(r(i, j)%v(i))
                r(i, j)%v = i*j
            end do
        end do
    end function mkarr_2d

end module derived_types_188_m

program derived_types_188
    use derived_types_188_m, only: item, mkarr, mkarr_n, mkarr_2d
    implicit none

    type(item), allocatable :: t(:)
    type(item), allocatable :: u(:, :)
    type(item) :: fixed(3)

    ! Unallocated allocatable LHS, constant-shape function result.
    t = mkarr()
    if (size(t) /= 3) error stop
    if (size(t(3)%v) /= 3) error stop
    if (sum(t(3)%v) /= 9) error stop

    ! Already allocated LHS of the wrong size, explicit-shape result.
    deallocate(t)
    allocate(t(1))
    t = mkarr_n(4)
    if (size(t) /= 4) error stop
    if (size(t(4)%v) /= 4) error stop
    if (sum(t(4)%v) /= 16) error stop

    ! Unallocated allocatable LHS, rank-2 result.
    u = mkarr_2d(3)
    if (size(u, 1) /= 3) error stop
    if (size(u, 2) /= 2) error stop
    if (sum(u(3, 2)%v) /= 18) error stop
    if (sum(u(2, 1)%v) /= 4) error stop

    ! Non-allocatable LHS keeps working.
    fixed = mkarr()
    if (size(fixed(3)%v) /= 3) error stop
    if (sum(fixed(3)%v) /= 9) error stop

    print *, "ok"
end program derived_types_188
