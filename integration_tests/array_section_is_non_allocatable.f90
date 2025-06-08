PROGRAM array_section_is_non_allocatable
    IMPLICIT NONE

    REAL, ALLOCATABLE, DIMENSION(:, :) :: fixup_counter
    INTEGER, PARAMETER :: x = 1

    allocate( fixup_counter(4, 2) )
    fixup_counter = 19.45

    CALL dim1_sweep ( fixup_counter (:, 1) )

CONTAINS

    SUBROUTINE dim1_sweep ( fixup_counter )
        REAL, DIMENSION(4), INTENT(INOUT) :: fixup_counter
        PRINT *, SIZE(fixup_counter)
        if (size(fixup_counter) /= 4) error stop
        print *, sum(fixup_counter)
        if (abs(sum(fixup_counter) - 77.8) > 1e-8) error stop
    END SUBROUTINE dim1_sweep
END PROGRAM array_section_is_non_allocatable

