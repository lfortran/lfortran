program associated_target_not_variable_1
    implicit none
    integer, pointer :: a(:)
    a => null()
    if (associated(a, 11)) print *, "bad"  ! {Error} 'target' argument of 'associated' intrinsic must be a pointer or target variable or function
end program
