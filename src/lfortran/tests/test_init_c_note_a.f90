! One of two modules whose C-backend object files test_init_c_note links;
! see run_c_note_test.cmake. Its bind(c) procedure is called from C.
module test_init_c_note_a
implicit none
integer :: na = 11
contains
subroutine test_init_c_note_get(r) bind(c, name="test_init_c_note_get")
integer, intent(out) :: r
r = na
end subroutine
end module
