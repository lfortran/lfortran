! Only the procedures of global_init_18_s.f90 use this module; see
! global_init_18.f90.
module global_init_18_m
    implicit none
    type :: t
        integer :: n = 7
        character(len=2) :: tag = "ab"
        integer, allocatable :: a(:)
    end type
    type(t) :: st
    integer, allocatable :: arr(:)
    integer :: counter = 0
end module global_init_18_m
