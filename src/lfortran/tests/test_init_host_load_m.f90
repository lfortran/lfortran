! The module state the bind(c) procedure of the library of
! run_host_load_test.cmake reaches only through an external procedure: its
! initializer, which static data cannot replace, runs only when the engine
! dispatches.
module test_init_host_load_m
implicit none
type :: leaf
    integer :: h = 0
    character(len=3) :: tag = "bad"
end type
type(leaf) :: arr(3) = leaf(7, "aaa")
end module
