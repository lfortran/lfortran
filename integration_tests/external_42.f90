program external_42
! An external procedure with an assumed-shape dummy, known only through an
! interface body and defined in C (external_42c.c), is used as a procedure
! pointer target and as an actual argument. Both reach the procedure by its
! own name.
implicit none
interface
    subroutine external_42_flag(a, flag)
    double precision, intent(inout) :: a(:)
    integer, intent(out) :: flag
    end subroutine
end interface
procedure(external_42_flag), pointer :: p
double precision :: a(4)
integer :: flag
a = 1
flag = 0
p => external_42_flag
call p(a, flag)
if (flag /= 42) error stop 1
flag = 0
call run(external_42_flag, flag)
if (flag /= 42) error stop 2
print *, flag
contains
    subroutine run(f, flag)
    procedure(external_42_flag) :: f
    integer, intent(out) :: flag
    double precision :: b(3)
    b = 2
    call f(b, flag)
    end subroutine
end program
