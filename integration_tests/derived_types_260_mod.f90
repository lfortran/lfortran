module derived_types_260_mod
    implicit none
    type :: qm
        integer :: n = 0
        real :: x = 1.5
        character(len=3) :: c = "dft"
    end type
    type(qm), parameter :: set(2) = [qm(6, 2.5, "abc"), qm(8, 3.5, "xyz")]
end module

