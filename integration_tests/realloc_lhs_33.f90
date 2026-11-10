! An allocatable function result that is first defined by an intrinsic
! assignment from an array expression (#3299).
program realloc_lhs_33
    implicit none
    integer :: i(7) = [1, 1, 2, 3, 5, 8, 13]
    integer :: x(4)
    integer, allocatable :: d(:)

    d = diff_1_int32(i)
    print *, d
    if (size(d) /= 6) error stop 1
    if (any(d /= [0, 1, 1, 2, 3, 5])) error stop 2

    d = diff_1_int32(i, 2)
    if (size(d) /= 5) error stop 3
    if (any(d /= [1, 0, 1, 1, 2])) error stop 4

    d = diff_1_int32(i, prepend=[0])
    if (size(d) /= 7) error stop 5
    if (any(d /= [1, 0, 1, 1, 2, 3, 5])) error stop 6

    x = [5, 11, 3, 23]
    if (any(func(x) /= [6, -8, 20])) error stop 7

contains

    function diff_1_int32(x, n, prepend, append) result(y)
        integer, intent(in) :: x(:)
        integer, intent(in), optional :: n
        integer, intent(in), optional :: prepend(:), append(:)
        integer, allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval_iint32(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if

        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend)
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append

        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
            integer :: work(size_work)
            if (size_prepend > 0) work(:size_prepend) = prepend
            work(size_prepend+1:size_prepend+size_x) = x
            if (size_append > 0) work(size_prepend+size_x+1:) = append

            do i = 1, n_
                work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
            end do

            y = work(1:size_work-n_)
        end block
    end function diff_1_int32

    elemental function optval_iint32(x, default) result(y)
        integer, intent(in), optional :: x
        integer, intent(in) :: default
        integer :: y
        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_iint32

    function func(i) result(o)
        integer, intent(in) :: i(:)
        integer, allocatable :: o(:)
        o = i(2:) - i(:size(i)-1)
    end function func

end program realloc_lhs_33
