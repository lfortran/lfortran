module class_172_m
    implicit none
    type :: t
        integer :: n = 7
    contains
        procedure :: show => t_show
        procedure :: get => t_get
    end type
    interface
        subroutine t_show(a, k)
            import :: t
            class(t), intent(in) :: a
            integer, intent(out) :: k
        end subroutine
        function t_get(a) result(r)
            import :: t
            class(t), intent(in) :: a
            integer :: r
        end function
    end interface
end module

subroutine t_show(a, k)
    use class_172_m, only: t
    implicit none
    class(t), intent(in) :: a
    integer, intent(out) :: k
    k = a%n
end subroutine

function t_get(a) result(r)
    use class_172_m, only: t
    implicit none
    class(t), intent(in) :: a
    integer :: r
    r = 2 * a%n
end function

program class_172
    use class_172_m
    implicit none
    type(t) :: x
    integer :: k
    k = 0
    call x%show(k)
    if (k /= 7) error stop
    if (x%get() /= 14) error stop
    x%n = 3
    k = 0
    call t_show(x, k)
    if (k /= 3) error stop
    if (t_get(x) /= 6) error stop
    print *, k, t_get(x)
end program
