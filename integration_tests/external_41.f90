program external_41
! A call to an external procedure known only through an interface block
! with an assumed-shape dummy calls the procedure by its own name, so it
! links to a definition in C (external_41c.c) or in another Fortran file
! (external_41b.f90).
implicit none
interface
    subroutine external_41_flag(a, flag)
    double precision, intent(inout) :: a(:)
    integer, intent(out) :: flag
    end subroutine
    subroutine external_41_sum(a, s)
    double precision, intent(inout) :: a(:)
    double precision, intent(out) :: s
    end subroutine
end interface
double precision :: a(4), s
integer :: flag
a = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
flag = 0
call external_41_flag(a, flag)
if (flag /= 42) error stop 1
call external_41_sum(a, s)
if (abs(s - 14.0d0) > 1.0d-12) error stop 2
if (any(abs(a - [2.0d0, 3.0d0, 4.0d0, 5.0d0]) > 1.0d-12)) error stop 3
call external_41_sum(a(1:4:2), s)
if (abs(s - 8.0d0) > 1.0d-12) error stop 4
if (any(abs(a - [3.0d0, 3.0d0, 5.0d0, 5.0d0]) > 1.0d-12)) error stop 5
print *, flag, s
end program
