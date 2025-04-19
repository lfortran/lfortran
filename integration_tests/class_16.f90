module stdlib_logger
    implicit none
    public :: logger_type
    type :: logger_type
        integer(4) :: max_width = 5
    end type logger_type
contains
subroutine format_output_string( self)
    class(logger_type) :: self
    ! call format_first_line() ! Does not work yet
contains
    subroutine format_first_line()
        if ( self % max_width == 5  ) then
            print *, "works"
        end if
    end subroutine format_first_line
end subroutine

end module

program class_16
    use stdlib_logger
    implicit none
    type(logger_type) :: logger
    call format_output_string(logger)
end program
