program logical_to_integer_cast
    implicit none
    
    logical :: logicalValue
    integer :: integerValue

    logical :: logicalArray(3)
    integer :: integerArray(3)
    
    ! Assign a logical value
    logicalValue = .TRUE.
    
    ! Perform an implicit logical to integer cast
    integerValue = logicalValue

    if (integerValue /= 1) error stop

    ! Assign logical array values
    logicalArray = [ .TRUE., .FALSE., .TRUE. ]
    ! Perform an implicit logical to integer cast for arrays
    integerArray = logicalArray

    if (any(integerArray /= [1, 0, 1])) error stop

end program logical_to_integer_cast
