! Note : gfortran fails for this, most of the other compiler don't
program string_58
    character(:), pointer :: str
    character(10), target :: str2

    print *, associated(str)
    if(associated(str) .neqv. .false.) error stop

    str=> str2
    
    print *, associated(str)
    if(associated(str) .neqv. .true.) error stop
end program