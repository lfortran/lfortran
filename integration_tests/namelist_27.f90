program namelist_27
    ! Test for handling triple-quoted strings (doubled quotes within strings)
    implicit none

    character(len=128) :: args
    namelist /expected/ args
    character(len=256) :: readme(3)

    args = ''
    readme(1) = '&EXPECTED'
    readme(2) = ' ARGS="""arg1"" ""-x"" ""and a long one"""'
    readme(3) = ' /'

    read(readme, nml=expected)
    print *, 'OK:', trim(args)

    if (trim(args) /= '"arg1" "-x" "and a long one"') then
        error stop 'Incorrect ARGS value read'
    end if

    ! Test with single quotes
    args = ''
    readme(1) = '&EXPECTED'
    readme(2) = " ARGS='''arg1'' ''-x'' ''and a long one'''"
    readme(3) = ' /'

    read(readme, nml=expected)
    print *, 'OK:', trim(args)

    if (trim(args) /= "'arg1' '-x' 'and a long one'") then
        error stop 'Incorrect ARGS value read with single quotes'
    end if

end program namelist_27
