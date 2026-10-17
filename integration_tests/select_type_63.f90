! A TYPE IS selector passed to a VALUE dummy: the dummy is a copy of the
! selector, including its allocatable and deferred-length components, so the
! callee's changes do not reach the selector (F2018 15.5.2.3).
module select_type_63_m
    implicit none
    type, abstract :: base
    end type
    type :: inner
        integer, allocatable :: q(:)
    end type
    type, extends(base) :: item
        integer :: v = 0
        integer, allocatable :: a(:)
        character(:), allocatable :: s
        type(inner) :: in
    end type
contains
    subroutine accept_value(x)
        type(item), value :: x
        if (x%v /= 8) error stop 1
        if (.not. allocated(x%a)) error stop 2
        if (any(x%a /= [1, 2, 3])) error stop 3
        if (x%s /= "abc") error stop 4
        if (any(x%in%q /= [7, 8])) error stop 5
        x%v = 100
        x%a(2) = 99
        x%s(1:1) = "Z"
        x%in%q(1) = -1
    end subroutine

    subroutine realloc_value(x)
        type(item), value :: x
        deallocate(x%a)
        allocate(x%a(5))
        x%a = 0
        x%s = "longer"
        deallocate(x%in%q)
    end subroutine
end module

program select_type_63
    use select_type_63_m
    implicit none
    class(base), allocatable :: x

    allocate(item :: x)
    select type (x)
    type is (item)
        x%v = 8
        allocate(x%a(3), x%in%q(2))
        x%a = [1, 2, 3]
        x%s = "abc"
        x%in%q = [7, 8]
        call accept_value(x)
        if (x%v /= 8) error stop 6
        if (any(x%a /= [1, 2, 3])) error stop 7
        if (x%s /= "abc") error stop 8
        if (any(x%in%q /= [7, 8])) error stop 9
        call realloc_value(x)
        if (size(x%a) /= 3) error stop 10
        if (any(x%a /= [1, 2, 3])) error stop 11
        if (x%s /= "abc") error stop 12
        if (.not. allocated(x%in%q)) error stop 13
        if (any(x%in%q /= [7, 8])) error stop 14
        print *, x%a, x%s, x%in%q
    end select
end program
