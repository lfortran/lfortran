module class_154_mod
    implicit none
    type :: base
        real :: x = 5
    end type
    type, extends(base) :: circle
        real :: r = 1
    end type
    type, extends(circle) :: ring
        integer :: n = 7
    end type
end module

program class_154
    use class_154_mod
    implicit none
    class(base), allocatable :: arr(:), arr2(:)
    class(*), allocatable :: u(:)
    integer :: i

    allocate(circle :: arr(3))
    select type (arr)
    type is (circle)
        do i = 1, 3
            if (abs(arr(i)%x - 5) > 1e-6) error stop
            if (abs(arr(i)%r - 1) > 1e-6) error stop
        end do
    class default
        error stop
    end select

    allocate(ring :: arr2(4))
    select type (arr2)
    type is (ring)
        do i = 1, 4
            if (abs(arr2(i)%x - 5) > 1e-6) error stop
            if (abs(arr2(i)%r - 1) > 1e-6) error stop
            if (arr2(i)%n /= 7) error stop
        end do
    class default
        error stop
    end select

    allocate(circle :: u(2))
    select type (u)
    type is (circle)
        do i = 1, 2
            if (abs(u(i)%x - 5) > 1e-6) error stop
            if (abs(u(i)%r - 1) > 1e-6) error stop
        end do
    class default
        error stop
    end select
end program
