! A table of procedure pointers: an array of a derived type whose procedure
! pointer components are given module procedures by structure constructors
! (lfortran/lfortran#2848). The array constant is the only reference to
! those procedures.
module procedure_pointer_30_funcs
implicit none
abstract interface
    integer function ifunc()
    end function
end interface
contains
    integer function one()
        one = 1
    end function
    integer function two()
        two = 2
    end function
end module

module procedure_pointer_30_mod
use procedure_pointer_30_funcs
implicit none
abstract interface
    subroutine void()
    end subroutine
end interface
type :: pp
    procedure(void), pointer, nopass :: pf => null()
end type
type :: pc
    procedure(ifunc), pointer, nopass :: f => null()
end type
integer :: cnt = 0
type(pp), parameter :: table(3) = [pp(a), pp(b), pp(c)]
type(pc), parameter :: ftable(2) = [pc(one), pc(two)]
type(pc) :: vtable(2) = [pc(two), pc(one)]
contains
    subroutine a()
        cnt = cnt + 1
    end subroutine
    subroutine b()
        cnt = cnt + 10
    end subroutine
    subroutine c()
        cnt = cnt + 100
    end subroutine
end module

program procedure_pointer_30
use procedure_pointer_30_mod
implicit none
integer :: i, s
do i = 1, 3
    call table(i)%pf()
end do
if (cnt /= 111) error stop
s = 0
do i = 1, 2
    s = 10*s + ftable(i)%f()
end do
if (s /= 12) error stop
s = 0
do i = 1, 2
    s = 10*s + vtable(i)%f()
end do
if (s /= 21) error stop
print *, cnt, s
end program
