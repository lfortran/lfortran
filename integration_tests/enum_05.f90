program enum_05
    implicit none
    integer :: enum_var1, enum_var2
    enum, bind(c)
        enumerator :: CAPS_and_small = 50 
        enumerator :: all_small = 100  
    end enum   
    ! Assign enum values to variables
    enum_var1 = all_small
    enum_var2 = CAPS_and_small
    print *, enum_var1, enum_var2
    if (enum_var1 /= 100) error stop
    if (enum_var2 /= 50) error stop
end program  