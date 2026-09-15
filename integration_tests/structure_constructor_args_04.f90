module structure_constructor_args_04_m
implicit none
type :: t
    character(len=3) :: c
    character(len=3) :: ca(2)
end type
type(t) :: g = t("hi", ["a", "b"])
type(t) :: h = t("hello", ["abcd", "efgh"])
end module

program structure_constructor_args_04
use structure_constructor_args_04_m
implicit none
type(t) :: lv = t("x", ["yy", "zz"])
type(t) :: a
call check(g, "hi ", "a  ", "b  ")
call check(h, "hel", "abc", "efg")
call check(lv, "x  ", "yy ", "zz ")
a = t("hi", ["a", "b"])
call check(a, "hi ", "a  ", "b  ")
a = t("hello", ["abcd", "efgh"])
call check(a, "hel", "abc", "efg")

contains

subroutine check(x, c, c1, c2)
    type(t), intent(in) :: x
    character(len=3), intent(in) :: c, c1, c2
    print '(3("[",a,"]"))', x%c, x%ca
    if (len(x%c) /= 3 .or. x%c /= c) error stop 1
    if (x%ca(1) /= c1 .or. x%ca(2) /= c2) error stop 2
    if (index(x%c // x%ca(1) // x%ca(2), achar(0)) /= 0) error stop 3
end subroutine

end program
