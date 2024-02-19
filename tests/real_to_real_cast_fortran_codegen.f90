program ImplicitRealToRealDifferentKindExample
    implicit none
    
    real :: realNumber1  ! Default real type (usually single precision)
    real(kind=8) :: realNumber2  ! Double precision real
    
    ! Assign a real number
    realNumber1 = 3.14
    
    ! Perform an implicit real to real cast with different kinds
    realNumber2 = realNumber1

end program ImplicitRealToRealDifferentKindExample
