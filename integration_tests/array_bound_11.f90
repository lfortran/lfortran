module array_bound_11_m
    implicit none
    type :: t
        integer :: k = 3
    contains
        procedure :: n
    end type
contains
    pure integer function n(self)
        class(t), intent(in) :: self
        n = self%k
    end function
end module

subroutine array_bound_11_s(o, a, total)
    use array_bound_11_m, only: t
    implicit none
    type(t), intent(in) :: o
    integer, intent(in) :: a(o%n())
    integer, intent(out) :: total
    total = sum(a) + 100*size(a)
end subroutine

program array_bound_11
    use array_bound_11_m, only: t
    implicit none
    interface
        subroutine array_bound_11_s(o, a, total)
            import :: t
            type(t), intent(in) :: o
            integer, intent(in) :: a(*)
            integer, intent(out) :: total
        end subroutine
    end interface
    type(t) :: o
    integer :: x(5), total
    x = [1, 2, 3, 4, 5]
    call array_bound_11_s(o, x, total)
    print *, total
    if (total /= 306) error stop
end program
