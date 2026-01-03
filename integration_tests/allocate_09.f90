program allocate_09

    implicit none
    call get_command_arguments_quoted()

contains

    subroutine get_command_arguments_quoted()
        character(len=:), allocatable :: arg
        integer :: ilength

        ilength = 10
        if( allocated(arg) ) deallocate(arg)
        allocate(character(len=ilength) :: arg)
        arg = 'okay fine'
        print *, arg, len(arg)
        if( arg /= 'okay fine' ) error stop
        if( len(arg) /= 9 ) error stop
    end subroutine get_command_arguments_quoted

end program allocate_09
