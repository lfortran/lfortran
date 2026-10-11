! The procedure pointer imports the function `nn` of its interface's bound
! for the program; a later implicit reference to `nn` still means the
! external function.
module proc_ptr_34_s
implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

module proc_ptr_34_m
implicit none
abstract interface
    subroutine cb(a)
        use proc_ptr_34_s, only: nn
        real :: a(nn(1))
    end subroutine
end interface
contains
    subroutine fa(a)
        use proc_ptr_34_s, only: nn
        real :: a(nn(1))
        a = 5.0
    end subroutine
end module

program proc_ptr_34
use proc_ptr_34_m, only: cb, fa
procedure(cb), pointer :: q
real :: z(2)
q => fa
call q(z)
k = nn(3)
print *, z, k
if (any(z /= 5.0)) error stop
if (k /= 300) error stop
end program

integer function nn(i)
integer, intent(in) :: i
nn = 100*i
end function
