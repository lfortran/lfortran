module select_rank_46_m
    implicit none
    type :: pt
        integer :: a = 1
    end type
contains
    integer function total_len(arr)
        integer, intent(in) :: arr(..)
        select rank (q => arr)
        rank (0)
            total_len = 1
        rank default
            total_len = size(q)
        end select
    end function

    integer function count_rank(a)
        real, intent(in) :: a(..)
        count_rank = rank(a)
    end function

    subroutine fill(a, v)
        integer, intent(inout) :: a(..)
        integer, intent(in) :: v
        select rank (a)
        rank (2)
            a = v
        rank (3)
            a = v
        end select
    end subroutine

    subroutine inspect(arr, r, n, s, lb, ub)
        real, intent(in) :: arr(..)
        integer, intent(out) :: r, n, s(3), lb(3), ub(3)
        integer :: i
        s = -1
        lb = -1
        ub = -1
        select rank (q => arr)
        rank (1)
            r = -1
            n = -1
        rank default
            r = rank(q)
            n = size(q)
            s(1:rank(q)) = shape(q)
            do i = 1, rank(q)
                lb(i) = lbound(q, i)
                ub(i) = ubound(q, i)
            end do
            if (size(q, rank(q)) /= s(rank(q))) error stop 10
            if (count_rank(q) /= rank(q)) error stop 11
        end select
    end subroutine

    subroutine set_all(arr, v)
        integer, intent(inout) :: arr(..)
        integer, intent(in) :: v
        select rank (q => arr)
        rank (0)
            q = v
        rank default
            call fill(q, v)
        end select
    end subroutine

    integer function count_pt(t)
        type(pt), intent(in) :: t(..)
        select rank (q => t)
        rank (0)
            count_pt = 0
        rank default
            count_pt = size(q)
        end select
    end function

    logical function ptr_assoc(p)
        integer, pointer, intent(in) :: p(..)
        select rank (q => p)
        rank (1)
            ptr_assoc = .false.
        rank default
            ptr_assoc = associated(q) .and. size(q) == 4
        end select
    end function
end module

program select_rank_46
    use select_rank_46_m
    implicit none
    integer :: box(3, 4), cube(2, 2, 2), s(3), lb(3), ub(3), r, n
    real :: x(3, 5), y(3, 1, 2)
    type(pt) :: t(2, 3)
    integer, pointer :: p2(:, :)

    box = 0
    if (total_len(box) /= 12) error stop 1
    if (total_len(cube) /= 8) error stop 2

    call inspect(x, r, n, s, lb, ub)
    print *, r, n, s, lb, ub
    if (r /= 2 .or. n /= 15) error stop 3
    if (any(s(1:2) /= [3, 5])) error stop 4
    if (any(lb(1:2) /= [1, 1]) .or. any(ub(1:2) /= [3, 5])) error stop 5

    call inspect(y, r, n, s, lb, ub)
    print *, r, n, s, lb, ub
    if (r /= 3 .or. n /= 6) error stop 6
    if (any(s /= [3, 1, 2]) .or. any(ub /= [3, 1, 2])) error stop 7

    call set_all(box, 7)
    if (any(box /= 7)) error stop 8
    call set_all(cube, 5)
    if (any(cube /= 5)) error stop 9

    if (count_pt(t) /= 6) error stop 12

    allocate(p2(2, 2))
    if (.not. ptr_assoc(p2)) error stop 13
    deallocate(p2)
    print *, "OK"
end program
