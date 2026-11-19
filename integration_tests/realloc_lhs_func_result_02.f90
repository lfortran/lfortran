! Reallocation on assignment of the explicit-shape array result of a
! function of character, derived and logical type to an allocatable
! variable, which takes the shape (and the length) of the result.
module realloc_lhs_func_result_02_m
    implicit none
    type :: item
        integer :: i = -1
        real, allocatable :: v(:)
    end type

contains

    function words(n) result(r)
        integer, intent(in) :: n
        character(len=3) :: r(n)
        integer :: i
        do i = 1, n
            r(i) = repeat(achar(iachar('a') + i - 1), 3)
        end do
    end function

    function items(n) result(r)
        integer, intent(in) :: n
        type(item) :: r(n)
        integer :: i
        do i = 1, n
            r(i)%i = i
            allocate(r(i)%v(i))
            r(i)%v = i
        end do
    end function

    function negated(a) result(r)
        logical, intent(in) :: a(:)
        logical :: r(size(a))
        r = .not. a
    end function

end module

program realloc_lhs_func_result_02
    use realloc_lhs_func_result_02_m
    implicit none
    character(len=3), allocatable :: s(:)
    character(len=:), allocatable :: d(:)
    type(item), allocatable :: t(:)
    logical, allocatable :: l(:)
    integer :: i

    s = words(2)
    if (size(s) /= 2 .or. s(1) /= "aaa" .or. s(2) /= "bbb") error stop "s unallocated"
    deallocate(s)
    allocate(s(0:2))
    s = words(3)
    if (lbound(s, 1) /= 0 .or. s(2) /= "ccc") error stop "s conforming"
    s = words(4)
    if (lbound(s, 1) /= 1 .or. size(s) /= 4 .or. s(4) /= "ddd") error stop "s nonconforming"

    d = words(3)
    if (len(d) /= 3 .or. size(d) /= 3 .or. d(3) /= "ccc") error stop "d unallocated"
    deallocate(d)
    allocate(character(len=5) :: d(3))
    d = words(3)
    if (len(d) /= 3 .or. size(d) /= 3 .or. d(1) /= "aaa") error stop "d other length"

    t = items(2)
    if (size(t) /= 2) error stop "t unallocated"
    if (t(2)%i /= 2 .or. size(t(2)%v) /= 2 .or. any(t(2)%v /= 2.)) error stop "t unallocated values"
    t = items(3)
    if (size(t) /= 3) error stop "t nonconforming"
    do i = 1, 3
        if (t(i)%i /= i .or. size(t(i)%v) /= i .or. any(t(i)%v /= i)) error stop "t values"
    end do
    deallocate(t)
    allocate(t(0:2))
    t = items(3)
    if (lbound(t, 1) /= 0 .or. t(2)%i /= 3 .or. size(t(2)%v) /= 3) error stop "t conforming"

    l = negated([.true., .false.])
    if (size(l) /= 2 .or. l(1) .or. .not. l(2)) error stop "l"
    l = negated(l)
    if (.not. l(1) .or. l(2)) error stop "l = negated(l)"
    print *, s, d, t(2)%i, l
end program
