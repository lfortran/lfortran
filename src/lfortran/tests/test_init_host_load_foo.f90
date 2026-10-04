! An external procedure of the library of run_host_load_test.cmake that
! reads test_init_host_load_m.
subroutine test_init_host_load_foo(r)
use test_init_host_load_m, only: arr
implicit none
integer, intent(out) :: r
r = arr(2)%h
end subroutine
