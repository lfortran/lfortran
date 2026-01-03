program ImplicitComplexToComplexDifferentKindExample
    implicit none
    
    complex :: complexValue1  ! Default complex type
    complex(kind=8) :: complexValue2  ! Complex type with higher precision
    
    ! Assign a complex value
    complexValue1 = (1.0, 2.0)  ! Complex number with default kind
    
    ! Perform an implicit complex to complex cast with different kinds
    complexValue2 = complexValue1
    
    ! Output the result
    print *, "Complex Value 1 (Default Kind):", complexValue1
    print *, "Complex Value 2 (Kind=8):", complexValue2
end program ImplicitComplexToComplexDifferentKindExample
