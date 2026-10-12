program func_call_in_decl_03
! Both translation units create the same function_call_in_declaration
! helpers for the result shapes; they must link together (#14534).
implicit none
interface
    function func_call_in_decl_03_f(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r(maxval(shape(a)))
    end function
    function func_call_in_decl_03_g(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r(sum(shape(a)))
    end function
end interface
integer :: y(3), z(3)
y = func_call_in_decl_03_f([1, 2, 3])
z = func_call_in_decl_03_g([1, 2, 3])
print *, y, z
if (any(y /= [2, 3, 4])) error stop
if (any(z /= [2, 4, 6])) error stop
end program
