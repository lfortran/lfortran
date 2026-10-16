module legacy_array_sections_28_mod
    ! An element of an allocatable array passed to an array dummy is
    ! sequence associated with the elements that follow it in array
    ! element order (#15332): module procedures, write-back, assumed-size
    ! dummies, derived types and characters.
    implicit none
    type :: pt
        integer :: a
        real :: b
    end type
contains
    subroutine scale_n(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n,n)
        x = x * 10
    end subroutine
    subroutine set_out(x)
        integer, intent(out) :: x(3)
        x = [7, 8, 9]
    end subroutine
    integer function sum_2x2(x)
        integer, intent(in) :: x(2,2)
        sum_2x2 = sum(x)
    end function
    subroutine check_as2(x, expected)
        integer, intent(in) :: x(2,*), expected(4)
        if (any([x(:,1), x(:,2)] /= expected)) error stop 4
    end subroutine
    subroutine negate_as2(x)
        integer, intent(inout) :: x(2,*)
        x(:,1:2) = -x(:,1:2)
    end subroutine
    subroutine negate_pt(x)
        type(pt), intent(inout) :: x(2,2)
        if (any(reshape(x%a, [4]) /= [2, 3, 4, 5])) error stop 6
        x%a = -x%a
    end subroutine
    subroutine check_chr(x)
        character(len=3), intent(in) :: x(2)
        if (x(1) /= "bbb" .or. x(2) /= "ccc") error stop 8
    end subroutine
    subroutine check_chr_star(x)
        character(len=*), intent(in) :: x(2)
        if (len(x) /= 3) error stop 9
        if (x(1) /= "bbb" .or. x(2) /= "ccc") error stop 10
    end subroutine
    subroutine set_chr(x)
        character(len=3), intent(inout) :: x(2)
        x = ["xxx", "yyy"]
    end subroutine
end module

program legacy_array_sections_28
    use legacy_array_sections_28_mod
    implicit none
    interface
        subroutine legacy_array_sections_28_ext(x, n)
            integer, intent(in) :: n
            integer, intent(inout) :: x(n,n)
        end subroutine
    end interface
    integer, allocatable :: b(:,:)
    type(pt), allocatable :: v(:,:)
    character(len=3), allocatable :: c(:,:)
    character(len=3) :: d(2,2)
    integer :: i

    allocate(b(2,3))
    b = reshape([(i, i = 1, 6)], [2, 3])
    call scale_n(b(2,1), 2)
    if (any(reshape(b, [6]) /= [1, 20, 30, 40, 50, 6])) error stop 1
    call set_out(b(1,2))
    if (any(reshape(b, [6]) /= [1, 20, 7, 8, 9, 6])) error stop 2
    if (sum_2x2(b(2,1)) /= 20 + 7 + 8 + 9) error stop 3

    b = reshape([(i, i = 1, 6)], [2, 3])
    call check_as2(b(2,1), [2, 3, 4, 5])
    call negate_as2(b(2,1))
    if (any(reshape(b, [6]) /= [1, -2, -3, -4, -5, 6])) error stop 5

    b = reshape([(i, i = 1, 6)], [2, 3])
    call legacy_array_sections_28_ext(b(2,1), 2)
    if (any(reshape(b, [6]) /= [1, -2, -3, -4, -5, 6])) error stop 7

    b = reshape([(i, i = 1, 6)], [2, 3])
    call check_as2_internal(b(2,1), [2, 3, 4, 5])
    call negate_as2_internal(b(2,1))
    if (any(reshape(b, [6]) /= [1, -2, -3, -4, -5, 6])) error stop 13

    allocate(v(2,3))
    do i = 1, 6
        v(mod(i - 1, 2) + 1, (i - 1)/2 + 1)%a = i
    end do
    call negate_pt(v(2,1))
    if (any(reshape(v%a, [6]) /= [1, -2, -3, -4, -5, 6])) error stop 11

    allocate(c(2,2))
    c = reshape(["aaa", "bbb", "ccc", "ddd"], [2, 2])
    call check_chr(c(2,1))
    call check_chr_star(c(2,1))
    call set_chr(c(2,1))
    if (c(1,1) /= "aaa" .or. c(2,1) /= "xxx") error stop 12
    if (c(1,2) /= "yyy" .or. c(2,2) /= "ddd") error stop 15

    deallocate(c)
    allocate(c(2,2))
    c = reshape(["aaa", "bbb", "ccc", "ddd"], [2, 2])
    call check_chr_internal(c(2,1))
    call set_chr_internal(c(2,1))
    if (c(1,1) /= "aaa" .or. c(2,1) /= "xxx") error stop 16
    if (c(1,2) /= "yyy" .or. c(2,2) /= "ddd") error stop 17

    d = reshape(["aaa", "bbb", "ccc", "ddd"], [2, 2])
    call check_chr(d(2,1))
    call check_chr_star(d(2,1))
    call set_chr(d(2,1))
    if (d(1,1) /= "aaa" .or. d(2,1) /= "xxx") error stop 18
    if (d(1,2) /= "yyy" .or. d(2,2) /= "ddd") error stop 19
    print *, "ok"
contains
    subroutine check_chr_internal(x)
        character(len=*), intent(in) :: x(2)
        if (x(1) /= "bbb" .or. x(2) /= "ccc") error stop 20
    end subroutine
    subroutine set_chr_internal(x)
        character(len=3), intent(inout) :: x(2)
        x = ["xxx", "yyy"]
    end subroutine
    subroutine check_as2_internal(x, expected)
        integer, intent(in) :: x(2,*), expected(4)
        if (any([x(:,1), x(:,2)] /= expected)) error stop 14
    end subroutine
    subroutine negate_as2_internal(x)
        integer, intent(inout) :: x(2,*)
        x(:,1:2) = -x(:,1:2)
    end subroutine
end program

subroutine legacy_array_sections_28_ext(x, n)
    integer, intent(in) :: n
    integer, intent(inout) :: x(n,n)
    x = -x
end subroutine
