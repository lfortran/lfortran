program ImplicitLogicalToIntCastExample
    implicit none
    
    logical :: logicalValue
    integer :: integerValue
    
    ! Assign a logical value
    logicalValue = .TRUE.
    
    ! Perform an implicit logical to integer cast
    integerValue = logicalValue
    print *, integerValue

end program ImplicitLogicalToIntCastExample
