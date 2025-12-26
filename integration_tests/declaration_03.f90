program declaration_03
    implicit none
    type(character(len=4)) :: str1 = 'abcd'
    type(real):: r1 = 1.53
    ! TODO: Uncomment for parameter initializations
    ! type(character(len=*)),parameter:: str2='(*(g0))'
    ! if (str2 /= '(*(g0))') error stop
    
    print *, str1, r1
    if (str1 /= 'abcd') error stop
    if ((r1 - 1.53) > 1e-4) error stop
end program declaration_03