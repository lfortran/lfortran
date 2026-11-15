! A class(t) assumed-rank dummy passed on to another class(t) assumed-rank dummy
module assumed_rank_class_02_m
    implicit none
    type :: t
        integer :: x = 0
    end type
    type, extends(t) :: u
        integer :: y = 5
    end type
contains
    integer function total_1(a) result(s)
        class(t), intent(in) :: a(:)
        integer :: i
        s = 0
        do i = 1, size(a)
            s = s + a(i)%x
        end do
        select type (a)
        type is (u)
            s = s + sum(a%y)
        end select
    end function

    integer function chk(a) result(r)
        class(t), intent(in) :: a(..)
        r = 100 * rank(a) + size(a)
        if (same_type_as(a, u())) r = r + 10000
        select rank (a)
        rank (1)
            r = r + 1000 * total_1(a)
        end select
    end function

    integer function fwd(a) result(r)
        class(t), intent(in) :: a(..)
        r = chk(a)
    end function
end module

program assumed_rank_class_02
    use assumed_rank_class_02_m
    implicit none
    type(t) :: ta(2), tb(2, 3)
    class(t), allocatable :: ca(:)
    integer :: i

    do i = 1, 2
        ta(i)%x = i
    end do
    if (fwd(ta) /= 3102) error stop 1

    if (fwd(tb) /= 206) error stop 2

    allocate(u :: ca(3))
    if (fwd(ca) /= 25103) error stop 3

    print *, "ok"
end program
