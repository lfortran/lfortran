function func_call_in_decl_03_f(a) result(r)
implicit none
integer, intent(in) :: a(:)
integer :: r(maxval(shape(a)))
r = a + 1
end function

function func_call_in_decl_03_g(a) result(r)
implicit none
integer, intent(in) :: a(:)
integer :: r(sum(shape(a)))
r = 2*a
end function
