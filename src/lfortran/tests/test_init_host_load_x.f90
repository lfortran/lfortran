! A module of the program of run_entry_load_test.cmake, whose records the
! engine dispatches from the program's constructor before the library is
! loaded.
module test_init_entry_load_x
implicit none
type :: node
    integer :: h = 0
    character(len=3) :: tag = "bad"
end type
type(node) :: nodes(3) = node(5, "xxx")
end module
