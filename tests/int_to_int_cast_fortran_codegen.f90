program ImplicitIntToIntCastExample
    implicit none
    
    integer :: integerValue1  ! Default integer type
    integer(kind=8) :: integerValue2  ! 8-byte integer
    
    ! Assign an integer value
    integerValue1 = 10
    
    ! Perform an implicit integer to integer cast
    integerValue2 = integerValue1
    
    ! Output the result
    print *, "Integer Value 1:", integerValue1
    print *, "Integer Value 2 (Kind=8):", integerValue2
end program ImplicitIntToIntCastExample
