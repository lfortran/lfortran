! Components of a derived-type `select rank` selector in its `rank (0)` block
module select_rank_component_01_m
    implicit none
    type :: t
        integer :: v = 0
        real :: y = 0
    end type
contains
    integer function cv(x)
        type(t), intent(in) :: x(..)
        cv = 0
        select rank (x)
        rank (0)
            cv = x%v
            if (x%v == 5) cv = cv + 100
        end select
    end function

    subroutine set(x)
        type(t), intent(inout) :: x(..)
        select rank (x)
        rank (0)
            x%y = 2.5
            x%v = x%v + 1
        end select
    end subroutine
end module

program select_rank_component_01
    use select_rank_component_01_m
    implicit none
    type(t) :: s
    s%v = 5
    if (cv(s) /= 105) error stop 1
    call set(s)
    if (s%v /= 6) error stop 2
    if (abs(s%y - 2.5) > 1e-6) error stop 3
    if (cv(s) /= 6) error stop 4
    print *, "ok"
end program
