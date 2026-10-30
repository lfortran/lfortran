program procedure_bounds_01
implicit none
abstract interface
    subroutine callback(values)
        integer, intent(inout) :: values(3)
    end subroutine
end interface
procedure(callback), pointer :: p
integer, allocatable :: values(:)
allocate(values(2))
values = 0
p => update
call p(values)
print *, "missing callback argument bounds check"
contains
subroutine update(values)
    integer, intent(inout) :: values(3)
    values(1) = 42
end subroutine
end program
