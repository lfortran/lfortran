PROGRAM array_section_is_non_allocatable
    IMPLICIT NONE

    REAL, ALLOCATABLE, DIMENSION(:, :) :: fixup_counter
    INTEGER, PARAMETER :: x = 1

    CALL dim1_sweep ( x, fixup_counter (:, 1))

CONTAINS

    SUBROUTINE dim1_sweep ( x_param, fixup_counter )
        REAL, DIMENSION(4), INTENT(INOUT) :: fixup_counter
        INTEGER, INTENT(IN) :: x_param
        
        PRINT *, x_param
        if (x_param /= 1) error stop
    END SUBROUTINE dim1_sweep
END PROGRAM array_section_is_non_allocatable
