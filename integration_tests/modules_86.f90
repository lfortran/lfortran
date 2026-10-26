! Two modules have `assignment(=)` with private specifics of the same name.
! A third module uses both, and a program uses the third module.
module modules_86_a
    implicit none
    private
    public :: ta, assignment(=)
    type :: ta
        integer :: x
    end type
    interface assignment(=)
        module procedure assign
    end interface
contains
    subroutine assign(l, r)
        type(ta), intent(out) :: l
        integer, intent(in) :: r
        l%x = r + 1
    end subroutine
end module

module modules_86_b
    implicit none
    private
    public :: tb, assignment(=)
    type :: tb
        integer :: y
    end type
    interface assignment(=)
        module procedure assign
    end interface
contains
    subroutine assign(l, r)
        type(tb), intent(out) :: l
        integer, intent(in) :: r
        l%y = r + 2
    end subroutine
end module

module modules_86_c
    use modules_86_a
    use modules_86_b
end module

program modules_86
    use modules_86_c
    implicit none
    type(ta) :: x
    type(tb) :: y
    x = 1
    y = 2
    print *, x%x, y%y
    if (x%x /= 2) error stop 1
    if (y%y /= 4) error stop 2
end program
