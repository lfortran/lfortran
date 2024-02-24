program ImplicitRealToIntCastExample
    implicit none
    
    real :: realNumber
    integer :: integerValue

    ! Assign a real number
    realNumber = 3.14

    ! Perform an implicit real to integer cast
    integerValue = realNumber

    ! Output the result
    print *, "Real Number:", realNumber
    print *, "Integer Value:", integerValue
end program ImplicitRealToIntCastExample
