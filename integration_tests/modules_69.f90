module modules_69_m
    implicit none
    type :: t
        integer :: n = 0
    end type t
    interface get_n
        module procedure :: get_n
    end interface
contains
    pure function get_n(s) result(r)
        type(t), intent(in) :: s
        integer :: r
        r = s%n
    end function
end module

module modules_69_m2
    use modules_69_m, only: t, get_n
    implicit none
contains
    subroutine f(x, a)
        type(t), intent(in) :: x
        character, intent(out) :: a(get_n(x))
        integer :: i
        do i = 1, get_n(x)
            a(i) = "x"
        end do
    end subroutine
end module

program modules_69
    use modules_69_m2, only: f
    use modules_69_m, only: t
    implicit none
    type(t) :: x
    character :: a(3)
    x%n = 3
    call f(x, a)
    if (a(1) /= "x") error stop
    if (a(2) /= "x") error stop
    if (a(3) /= "x") error stop
    print *, "PASS"
end program
