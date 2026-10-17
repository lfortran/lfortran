program shared_library_01
! Calls procedures from a shared library that is linked by the compiler
! driver with `--shared` (see `SHARED_LIBRARY_FILES` in CMakeLists.txt)
use iso_c_binding, only: c_int
use shared_library_01_mod, only: add_one, counter
implicit none
interface
    subroutine shared_library_01_scale(x, n) bind(c, name="shared_library_01_scale")
    import :: c_int
    integer(c_int), value :: n
    integer(c_int), intent(inout) :: x(n)
    end subroutine
end interface
integer(c_int) :: a(3)

if (add_one(41) /= 42) error stop
if (add_one(1) /= 2) error stop
if (counter /= 2) error stop

a = [1, 2, 3]
call shared_library_01_scale(a, 3_c_int)
if (any(a /= [2, 4, 6])) error stop
print *, "ok"
end program
