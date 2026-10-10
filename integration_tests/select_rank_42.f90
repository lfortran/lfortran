! `select rank (assoc => selector)` with a `type(T)` assumed-rank selector:
! in the `rank (0)` block the associate name is a scalar of the derived type.
module select_rank_42_mod
    implicit none

    type :: item_type
        integer :: value
        real :: weight
    end type

    interface
        module subroutine consume(value, total)
            type(item_type), intent(in) :: value(..)
            integer, intent(out) :: total
        end subroutine consume
    end interface

contains

    subroutine bump(value)
        type(item_type), intent(inout) :: value(..)
        select rank (item => value)
        rank (0)
            item%value = item%value + 1
            item%weight = 2.0 * item%weight
        rank (1)
            item(:)%value = item(:)%value + 10
        rank default
            error stop
        end select
    end subroutine bump

    function get_value(value) result(r)
        type(item_type), intent(in) :: value(..)
        integer :: r
        r = -1
        select rank (it => value)
        rank (0)
            r = it%value
        rank (2)
            r = it(2, 1)%value
        end select
    end function get_value

end module select_rank_42_mod

submodule (select_rank_42_mod) select_rank_42_submod
contains

    module subroutine consume(value, total)
        type(item_type), intent(in) :: value(..)
        integer, intent(out) :: total

        select rank (item => value)
        rank (0)
            total = item%value
        rank (1)
            total = sum(item%value)
        rank default
            error stop
        end select
    end subroutine consume

end submodule select_rank_42_submod

program select_rank_42
    use select_rank_42_mod
    implicit none
    type(item_type) :: s
    type(item_type) :: a(3)
    type(item_type) :: b(2, 2)
    integer :: total

    s = item_type(7, 1.5)
    a = [item_type(1, 0.0), item_type(2, 0.0), item_type(3, 0.0)]
    b = reshape([item_type(11, 0.0), item_type(12, 0.0), &
                 item_type(13, 0.0), item_type(14, 0.0)], [2, 2])

    call consume(s, total)
    if (total /= 7) error stop

    call consume(a, total)
    if (total /= 6) error stop

    call bump(s)
    if (s%value /= 8) error stop
    if (abs(s%weight - 3.0) > 1.0e-6) error stop

    call bump(a)
    if (any(a%value /= [11, 12, 13])) error stop

    if (get_value(s) /= 8) error stop
    if (get_value(b) /= 12) error stop

    print *, "ok"
end program select_rank_42
