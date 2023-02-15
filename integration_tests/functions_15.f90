module module_1
    implicit none

contains

    subroutine sub_1(a, b, c, d, e, f)
        integer, intent(inout) :: a
        real, intent(inout) :: b
        logical, intent(in) :: c
        character(*), intent(in), optional :: d
        integer, intent(in), optional :: e
        real, intent(in) :: f
        a = b ** 2
        b = b * f
    end subroutine sub_1
end module module_1

program functions_15
    use module_1, only: sub_1
    implicit none
    integer :: a
    real :: b
    a = 1
    b = 2
    call sub_1(a, b, .true., d="12", e=5, f=6.0)
    call sub_1(a, b, .false., f=6.0)
    call sub_1(a, b, .true., e=78, f=117.0)
    call sub_1(a, b, .false., d="a", f=123.0)
    if (a /= 70963776) error stop
    if (b /= 1036152.00) error stop
end program functions_15
