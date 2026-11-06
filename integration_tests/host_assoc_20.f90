module host_assoc_20_module
implicit none
abstract interface
    subroutine callback()
    end subroutine
end interface
contains
subroutine invoke(f)
    procedure(callback) :: f
    call f()
end subroutine
end module
program host_assoc_20
use host_assoc_20_module
implicit none
integer :: n, expected
abstract interface
    subroutine writer(values)
        import n
        integer, intent(out) :: values(n)
    end subroutine
end interface
procedure(writer), pointer :: p
n = 3
expected = n
p => fill
call invoke(worker)
n = 5
expected = 2 * n
p => fill_twice
call invoke(worker)
contains
subroutine fill(values)
    integer, intent(out) :: values(n)
    values = n
end subroutine
subroutine fill_twice(values)
    integer, intent(out) :: values(n)
    values = 2 * n
end subroutine
subroutine worker()
    integer :: values(n)
    call p(values)
    if (size(values) /= n) error stop 1
    if (any(values /= expected)) error stop 2
end subroutine
end program
