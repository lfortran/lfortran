! The value of a named constant whose procedure pointer component is
! associated with a procedure names that procedure, which need not be
! accessible where the named constant is referenced (lfortran/lfortran#2848).
module procedure_pointer_32_funcs
implicit none
abstract interface
    function retchar()
        character(len=1) :: retchar
    end function
end interface
contains
    function a()
        character(len=1) :: a
        a = 'a'
    end function
    function b()
        character(len=1) :: b
        b = 'b'
    end function
end module

module procedure_pointer_32_mod
use procedure_pointer_32_funcs
implicit none
private
public :: pc, table, first, letter
type :: pc
    procedure(retchar), pointer, nopass :: rc => null()
end type
type(pc), parameter :: table(2) = [pc(a), pc(b)]
type(pc), parameter :: first = pc(a)
contains
    function letter(x)
        type(pc), intent(in) :: x
        character(len=1) :: letter
        letter = x%rc()
    end function
end module

program procedure_pointer_32
implicit none
block
    use procedure_pointer_32_mod, only: table
    if (table(1)%rc()//table(2)%rc() /= 'ab') error stop
    print *, table(1)%rc()//table(2)%rc()
end block
block
    use procedure_pointer_32_mod, only: first, letter
    if (letter(first) /= 'a') error stop
    print *, letter(first)
end block
end program
