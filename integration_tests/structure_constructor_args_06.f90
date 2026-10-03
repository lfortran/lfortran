! `reduce` on a derived type starts every component from zero: the
! components of the type it extends, every element of an array of a derived
! type, every element of a `dim=` result, and a large array component, which
! is zeroed without being built element by element.
module structure_constructor_args_06_m
    implicit none
    integer, parameter :: big_n = 100000
    type :: base
        integer :: k
        integer :: b(2)
    end type
    type :: inner
        integer :: q(2)
        character(len=2) :: c
    end type
    type, extends(base) :: t
        integer :: n
        type(inner) :: in(2)
    end type
    type :: big_t
        integer :: n
        real :: big(big_n)
    end type
contains
    pure function add(x, y) result(r)
        type(t), intent(in) :: x, y
        type(t) :: r
        r%k = x%k + y%k
        r%b = x%b + y%b
        r%n = x%n + y%n
        r%in(1)%q = x%in(1)%q + y%in(1)%q
        r%in(2)%q = x%in(2)%q + y%in(2)%q
        r%in(1)%c = y%in(1)%c
        r%in(2)%c = y%in(2)%c
    end function

    pure function add_big(x, y) result(r)
        type(big_t), intent(in) :: x, y
        type(big_t) :: r
        r%n = x%n + y%n
        r%big = x%big + y%big
    end function
end module

program structure_constructor_args_06
    use structure_constructor_args_06_m
    implicit none
    type(t) :: arr(3), s, arr2(3, 2), sd(2)
    type(big_t), allocatable :: big_arr(:), big_s
    integer :: i, j

    do i = 1, 3
        arr(i)%k = 10*i
        arr(i)%b = i
        arr(i)%n = i
        arr(i)%in(1)%q = i
        arr(i)%in(2)%q = 2*i
        arr(i)%in(1)%c = "ab"
        arr(i)%in(2)%c = "cd"
    end do
    s = reduce(arr, add)
    print *, s%k, s%b, s%n, s%in(1)%q, s%in(2)%q, s%in(1)%c, s%in(2)%c
    if (s%k /= 60) error stop 1
    if (any(s%b /= 6)) error stop 2
    if (s%n /= 6) error stop 3
    if (any(s%in(1)%q /= 6)) error stop 4
    if (any(s%in(2)%q /= 12)) error stop 5
    if (s%in(1)%c /= "ab" .or. s%in(2)%c /= "cd") error stop 6

    do j = 1, 2
        do i = 1, 3
            arr2(i, j)%k = 10*i
            arr2(i, j)%b = j
            arr2(i, j)%n = i
            arr2(i, j)%in(1)%q = i
            arr2(i, j)%in(2)%q = j
            arr2(i, j)%in(1)%c = "ab"
            arr2(i, j)%in(2)%c = "cd"
        end do
    end do
    sd = reduce(arr2, add, dim=1)
    print *, sd%k, sd(1)%b, sd(2)%b, sd%n, sd(1)%in(2)%q, sd(2)%in(2)%q
    do j = 1, 2
        if (sd(j)%k /= 60) error stop 9
        if (any(sd(j)%b /= 3*j)) error stop 10
        if (sd(j)%n /= 6) error stop 11
        if (any(sd(j)%in(1)%q /= 6)) error stop 12
        if (any(sd(j)%in(2)%q /= 3*j)) error stop 13
        if (sd(j)%in(1)%c /= "ab" .or. sd(j)%in(2)%c /= "cd") error stop 14
    end do

    allocate(big_arr(3), big_s)
    do i = 1, 3
        big_arr(i)%n = i
        big_arr(i)%big = i
    end do
    big_s = reduce(big_arr, add_big)
    print *, big_s%n, big_s%big(1), big_s%big(big_n)
    if (big_s%n /= 6) error stop 7
    if (any(big_s%big /= 6.0)) error stop 8
end program
