module separate_compilation_15a_module
implicit none
private
public :: test_solver
contains

subroutine test_solver()

real :: xl(2)
real :: xu(2)

call recursive_fun2()

print *, "xl = ", xl
if ( abs(sum(xl) - (-7.0)) > 1e-8 ) error stop
print *, "xu = ", xu
if ( abs(sum(xu) - 2.0) > 1e-8 ) error stop

contains

subroutine take_xl_xu(xl, xu)

implicit none
real, intent(out) :: xl(:)
real, intent(out) :: xu(:)

xl = [12.0, -19.0]
xu = [1.0, 1.0]
end subroutine

subroutine recursive_fun2()
implicit none
call take_xl_xu(xl, xu)
end subroutine recursive_fun2

end subroutine test_solver


end module separate_compilation_15a_module
