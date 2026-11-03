! `implicit none (external)` requires every external procedure referenced in
! the scoping unit, its contained subprograms and BLOCK constructs to have the
! EXTERNAL attribute or an explicit interface (F2018 C895).
module implicit_none_external_01_mod
implicit none (type, external)
contains
subroutine module_procedure()
call foo()
end subroutine
end module

subroutine block_construct()
implicit none (external)
block
call foo()
end block
end subroutine

subroutine host()
implicit none (external)
call contained()
contains
subroutine contained()
call foo()
end subroutine
end subroutine

subroutine function_reference()
implicit none (external)
real :: f
x = g(2.0)
x = f(1.0)
end subroutine

subroutine dummy_function(d)
implicit none (external)
real :: d
x = d(1.0)
end subroutine

! A unit without the statement is not affected.
subroutine not_inherited()
call foo()
x = g(2.0)
end subroutine

program implicit_none_external_01
implicit none (external)
call foo()
end program

subroutine foo()
end subroutine

real function f(x)
real, intent(in) :: x
f = x
end function

real function g(x)
real, intent(in) :: x
g = x
end function

! A duplicate IMPLICIT NONE is reported, and the call is still checked.
subroutine duplicate_implicit_none()
implicit none
implicit none (external)
call foo()
end subroutine

! Last, because no error is reported after one in a procedure with ENTRY
! statements (#16680).
subroutine with_entry(n)
implicit none (type, external)
integer, intent(in) :: n
call foo()
return
entry an_entry(n)
end subroutine
