! The bind(c) procedure of the library of run_host_load_test.cmake: it uses
! no module itself, so only the startup initializes what it reaches.
subroutine test_init_host_load_e(r) bind(c, name="test_init_host_load_e")
use iso_c_binding, only: c_int
implicit none
integer(c_int), intent(out) :: r
integer :: v
external :: test_init_host_load_foo
call test_init_host_load_foo(v)
r = v
end subroutine
