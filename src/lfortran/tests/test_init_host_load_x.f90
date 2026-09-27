! A module of the program of run_host_load_test.cmake, whose records are
! initialized, by the program's constructor and its lfortran_initialize(),
! before the library is loaded.
module test_init_host_load_x
implicit none
type :: node
    integer :: h = 0
    character(len=3) :: tag = "bad"
end type
type(node) :: nodes(3) = node(5, "xxx")
end module
