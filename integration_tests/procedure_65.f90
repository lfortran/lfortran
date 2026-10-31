module procedure_65_m
implicit none
integer :: evaluations = 0
abstract interface
    subroutine callback(values)
        integer, intent(inout) :: values(3)
    end subroutine
end interface
contains
integer function first()
    evaluations = evaluations + 1
    first = 2
end function
subroutine update(values)
    integer, intent(inout) :: values(3)
    values = values + 1
end subroutine
integer function total(values)
    integer, intent(in) :: values(3)
    total = sum(values)
end function
subroutine invoke(f, values)
    procedure(callback) :: f
    integer, intent(inout) :: values(:)
    call f(values(first():))
end subroutine
end module

program procedure_65
use procedure_65_m
implicit none
procedure(callback), pointer :: p
procedure(total), pointer :: q
integer, allocatable :: values(:)
allocate(values(4))
values = 0
p => update
call p(values(first():))
if (evaluations /= 1 .or. any(values /= [0, 1, 1, 1])) error stop 1
call invoke(p, values)
if (evaluations /= 2 .or. any(values /= [0, 2, 2, 2])) error stop 2
q => total
if (q(values(first():)) /= 6) error stop 3
if (evaluations /= 3) error stop 4
end program
