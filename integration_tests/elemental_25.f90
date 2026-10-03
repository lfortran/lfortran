module elemental_25_m
    implicit none
    type :: s_t
        integer, allocatable :: v(:)
    end type
    type :: p_t
        real :: x
    contains
        procedure :: dbl
    end type
contains
    elemental function dbl(self) result(r)
        class(p_t), intent(in) :: self
        real :: r
        r = 2 * self%x
    end function

    elemental function add(a, b) result(r)
        integer, intent(in) :: a, b
        integer :: r
        r = a + b
    end function

    elemental function f(x) result(r)
        real, intent(in) :: x
        real :: r
        r = 2 * x
    end function

    elemental function to_s(x) result(r)
        real, intent(in) :: x
        type(s_t) :: r
        allocate(r%v(1))
        r%v(1) = int(x)
    end function

    integer function n_elements(a)
        real, intent(in) :: a(:)
        n_elements = size(a)
    end function

    subroutine check_2d(x)
        real, intent(in) :: x(:,:)
        real, allocatable :: row(:)
        type(s_t), allocatable :: s(:)
        integer :: j

        if (size(f(x(1,:))) /= 3) error stop
        if (n_elements(f(x(2,:))) /= 3) error stop
        if (n_elements(f(x(:,3))) /= 2) error stop
        if (size(f(x(2,2:3))) /= 2) error stop
        if (any(shape(f(x(:,2:3))) /= [2, 2])) error stop
        if (size(f(x(1,1:3:2))) /= 2) error stop
        if (any(f(x(1,1:3:2)) /= [2 * x(1,1), 2 * x(1,3)])) error stop
        if (any(f(x(2,3:1:-1)) /= [2 * x(2,3), 2 * x(2,2), 2 * x(2,1)])) error stop

        row = f(x(2,:))
        if (size(row) /= 3) error stop
        if (any(row /= 2 * x(2,:))) error stop

        s = to_s(x(1,:))
        if (size(s) /= 3) error stop
        do j = 1, 3
            if (s(j)%v(1) /= int(x(1,j))) error stop
        end do
    end subroutine

    subroutine check_3d(y)
        real, intent(in) :: y(:,:,:)
        if (size(f(y(1,:,2))) /= 3) error stop
        if (any(shape(f(y(1,:,:))) /= [3, 4])) error stop
        if (any(shape(f(y(:,2,:))) /= [2, 4])) error stop
        if (sum(f(y(2,3,:))) /= 2 * sum(y(2,3,:))) error stop
    end subroutine

    subroutine check_type_bound(a)
        type(p_t), intent(in) :: a(:,:)
        if (size(a(2,:)%dbl()) /= 3) error stop
        if (any(a(2,:)%dbl() /= [4., 8., 12.])) error stop
    end subroutine
end module

program elemental_25
    use elemental_25_m
    implicit none
    real :: x(2,3), y(2,3,4)
    type(p_t) :: a(2,3)
    integer :: mm(2,3), v(3)
    integer :: i

    x = reshape([(real(i), i = 1, 6)], [2, 3])
    y = reshape([(real(i), i = 1, 24)], [2, 3, 4])
    call check_2d(x)
    call check_3d(y)
    a%x = x
    call check_type_bound(a)

    mm = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    v = add(mm(1,:), 1)
    print *, v
    if (any(v /= [2, 4, 6])) error stop
    print *, "ok"
end program
