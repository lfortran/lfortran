module m_cli2

implicit none

contains

subroutine check_commandline()

    call default_help()

contains

    subroutine default_help()
        integer :: ilength
        call get_command_argument(number=0, length=ilength)
    end subroutine default_help
end subroutine check_commandline

end module m_cli2
