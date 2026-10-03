module structure_constructor_args_03_m
implicit none
type :: t
    character(len=2) :: ca(2) = ["xx", "yy"]
    integer :: n(2) = 0
end type
type(t) :: g = t("cd", 5)
end module

program structure_constructor_args_03
use structure_constructor_args_03_m
implicit none
type(t) :: lv = t("ef", 6)
type(t) :: a
character(len=2) :: s
call check(g, "cd", 5)
call check(lv, "ef", 6)
a = t("gh", 7)
call check(a, "gh", 7)
s = "ij"
a = t(s, 8)
call check(a, "ij", 8)

contains

subroutine check(x, c, n)
    type(t), intent(in) :: x
    character(len=2), intent(in) :: c
    integer, intent(in) :: n
    print *, x%ca, x%n
    if (any(x%ca /= c)) error stop 1
    if (any(x%n /= n)) error stop 2
end subroutine

end program
