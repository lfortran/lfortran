! Components and bindings of a derived-type `select rank` selector in its
! `rank (0)` block
module select_rank_component_01_m
    implicit none
    type :: t
        integer :: v = 0
        real :: y = 0
        integer :: arr(3) = 0
    contains
        procedure :: bump
        procedure :: get
    end type
contains
    subroutine bump(self)
        class(t), intent(inout) :: self
        self%v = self%v + 10
    end subroutine

    integer function get(self)
        class(t), intent(in) :: self
        get = self%v
    end function

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

    integer function elem(x)
        type(t), intent(inout) :: x(..)
        elem = 0
        select rank (x)
        rank (0)
            x%arr(2) = 7
            elem = x%arr(2) + x%arr(3)
        end select
    end function

    integer function bound(x)
        type(t), intent(inout) :: x(..)
        bound = 0
        select rank (x)
        rank (0)
            call x%bump()
            bound = x%get()
        end select
    end function
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
    s%arr = [1, 2, 3]
    if (elem(s) /= 10) error stop 5
    if (any(s%arr /= [1, 7, 3])) error stop 6
    if (bound(s) /= 16) error stop 7
    if (s%v /= 16) error stop 8
    print *, "ok"
end program
