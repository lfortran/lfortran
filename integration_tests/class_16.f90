module stdlib_logger
    implicit none
    public :: logger_type
    type :: logger_type
        integer(4) :: max_width = 5
    end type logger_type
contains
subroutine format_output_string( self)
    class(logger_type) :: self
    call format_first_line()
    if (self%max_width /= 15) error stop
contains
    subroutine format_first_line()
        if (self%max_width /= 10) error stop
        self%max_width = 15
    end subroutine format_first_line
end subroutine

end module

program class_16
    use stdlib_logger
    implicit none
    type(logger_type) :: logger
    logger%max_width = 10
    call format_output_string(logger)
end program
