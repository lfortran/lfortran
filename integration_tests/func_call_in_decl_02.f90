module func_call_in_decl_02_mod
implicit none
contains

! The result's element length refers to the dummy `xx`, and its shape is a
! function call, so function_call_in_declaration rewrites the result type.
function same(xx) result(yy)
    character(len=*), intent(in) :: xx(:)
    character(len=len(xx)) :: yy(size(xx))
    yy = xx
end function same

end module func_call_in_decl_02_mod

program func_call_in_decl_02
use func_call_in_decl_02_mod
implicit none
character(len=3) :: a(2)
character(len=3) :: b(2)
a = ['abc', 'def']
b = same(a)
print *, b
if (b(1) /= 'abc') error stop
if (b(2) /= 'def') error stop
if (len(same(a)) /= 3) error stop
end program func_call_in_decl_02
