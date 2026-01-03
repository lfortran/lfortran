module kwargs_02_stdlib_logger
type :: logger_type

contains

    private

    procedure, public, pass(self) :: add_log_file
    procedure, public, pass(self) :: log_io_error

end type logger_type

contains

    subroutine add_log_file(self, filename, unit)
        class(logger_type), intent(inout) :: self
        character(*), intent(in) :: filename
        integer, intent(out), optional :: unit
        call self % log_io_error(1, filename=filename, unit=unit)
    end subroutine add_log_file

    subroutine log_io_error(self, code, filename, unit)
        class(logger_type), intent(inout)  :: self
        integer, intent(in) :: code
        character(*), intent(in), optional :: filename
        integer, intent(out), optional :: unit
    end subroutine log_io_error

end module kwargs_02_stdlib_logger

program kwargs_02
end program
