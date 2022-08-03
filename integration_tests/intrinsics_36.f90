program intrinsics_36
    implicit none
    character(len=25) :: empty = adjustl('')
    character(len=25) :: all_spaces = adjustl('    ')
    character(len=25) :: simple = adjustl('gfortran')
    character(len=25) :: space_at_start = adjustl('   gfortran')
    character(len=25) :: space_at_end = adjustl('gfortran   ')
    character(len=25) :: space_in_between = adjustl('   g for tran   ')
    character(len=25) :: spaces_with_symbols = adjustl('  # gfor* t $ ran &    ')

    print *, empty
    if (empty /= '') error stop

    print *, all_spaces
    if (all_spaces /= '    ') error stop

    print *, simple
    if (simple /= 'gfortran') error stop

    print *, space_at_start
    if (space_at_start /= 'gfortran   ') error stop

    print *, space_at_end
    if (space_at_end /= 'gfortran   ') error stop

    print *, space_in_between
    if (space_in_between /= 'g for tran      ') error stop

    print *, spaces_with_symbols
    if (spaces_with_symbols /= '# gfor* t $ ran &      ') error stop

end program
