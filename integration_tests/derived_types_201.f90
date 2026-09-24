module derived_types_201_m
    implicit none

    type :: item
        integer, allocatable :: v(:)
    end type item

    abstract interface
        function mkarr_2d_i(n) result(r)
            import :: item
            integer, intent(in) :: n
            type(item) :: r(n, 2)
        end function mkarr_2d_i

        function mkarr_1d_i(n) result(r)
            integer, intent(in) :: n
            integer :: r(n)
        end function mkarr_1d_i
    end interface

    type :: holder
        procedure(mkarr_2d_i), pointer, nopass :: p2 => null()
        procedure(mkarr_1d_i), pointer, nopass :: p1 => null()
    end type holder

contains

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

    function mkarr_1d(n) result(r)
        integer, intent(in) :: n
        integer :: r(n)
        integer :: i
        do i = 1, n
            r(i) = i*10
        end do
    end function mkarr_1d

    subroutine through_dummy(f)
        procedure(mkarr_1d_i) :: f
        integer, allocatable :: t(:)
        integer :: i
        t = f(5)
        if (size(t) /= 5) error stop
        do i = 1, 5
            if (t(i) /= i*10) error stop
        end do
    end subroutine through_dummy

end module derived_types_201_m

program derived_types_201
    use derived_types_201_m, only: item, holder, mkarr_1d, mkarr_2d, through_dummy
    implicit none

    type(holder) :: h
    type(item), allocatable :: u(:, :)
    integer, allocatable :: t(:)
    integer, allocatable :: s(:)
    integer :: i, j

    h%p2 => mkarr_2d
    h%p1 => mkarr_1d

    ! rank 2, derived type with an allocatable component
    u = h%p2(3)
    if (size(u, 1) /= 3) error stop
    if (size(u, 2) /= 2) error stop
    do j = 1, 2
        do i = 1, 3
            if (size(u(i, j)%v) /= i) error stop
            if (u(i, j)%v(1) /= i*j) error stop
        end do
    end do

    ! rank 1
    t = h%p1(4)
    if (size(t) /= 4) error stop
    do i = 1, 4
        if (t(i) /= i*10) error stop
    end do

    ! left hand side already allocated with the right shape
    allocate(s(4))
    s = h%p1(4)
    if (size(s) /= 4) error stop
    do i = 1, 4
        if (s(i) /= i*10) error stop
    end do

    ! the same result returned through a dummy procedure
    call through_dummy(mkarr_1d)

    print *, "ok"
end program derived_types_201
