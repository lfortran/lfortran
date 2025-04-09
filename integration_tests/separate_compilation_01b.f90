module separate_compilation_01_module
implicit none
real :: val

contains
subroutine set_val(x)
real :: x
val = x
print *, "value of val is set to ", val
end subroutine set_val

subroutine get_val(x)
real, intent(out) :: x
x = val
end subroutine get_val

subroutine test_val(x)
real :: x
print *, "testing val"
print *, "val = ", val
print *, "x = ", x
if (abs(val - x) > 1e-8) error stop
end subroutine test_val
end module
