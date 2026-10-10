program separate_compilation_59
use separate_compilation_59a
implicit none
type(t) :: x
integer :: n
call reg(x, g)
if (.not. associated(x%f)) error stop
call x%f("abcde", n)
if (n /= 5) error stop
print *, n
contains
subroutine g(name, n)
    character(len=*), intent(in) :: name
    integer, intent(out) :: n
    if (name /= "abcde") error stop
    n = len(name)
end subroutine
end program
