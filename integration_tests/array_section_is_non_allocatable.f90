PROGRAM array_section_is_non_allocatable
    IMPLICIT NONE

    REAL, ALLOCATABLE, DIMENSION(:, :) :: fixup_counter

    CALL dim1_sweep ( fixup_counter (:, 1))

CONTAINS

    SUBROUTINE dim1_sweep ( fixup_counter )
       REAL, DIMENSION(4), INTENT(INOUT) :: fixup_counter
    END SUBROUTINE dim1_sweep
END PROGRAM array_section_is_non_allocatable
