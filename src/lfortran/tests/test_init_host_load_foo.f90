! An external procedure of the library of run_entry_load_test.cmake that
! reads test_init_entry_load_m.
subroutine test_init_entry_load_foo(r)
use test_init_entry_load_m, only: arr
implicit none
integer, intent(out) :: r
r = arr(2)%h
end subroutine
