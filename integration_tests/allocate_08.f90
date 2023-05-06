program allocate_08

    implicit none
    call get_command_arguments_quoted()

contains

    subroutine get_command_arguments_quoted()
        character(len=:),allocatable :: arg
        integer                      :: ilength
        ilength=10

        if(allocated(arg))deallocate(arg)
        allocate(character(len=ilength) :: arg)
        arg = 'okay fine'
        print *, arg, len(arg)
    end subroutine get_command_arguments_quoted


end program allocate_08
