! Test to ensure that 'type' declaration without
! a 'program' statement at the beginning parses
! correctly
    implicit none
    type :: test_t
        real :: a
    end type
end
