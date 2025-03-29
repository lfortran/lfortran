module class_15_mod
    type :: logger_type
    contains
        private
        procedure, public, pass(self) :: add_log_file
    
    end type logger_type
    contains
        subroutine add_log_file(self, filename, unit)
            class(logger_type), intent(inout) :: self
            character(*), optional :: filename
            integer, optional :: unit
            if (present(filename)) filename = "lfortran"
            if (present(unit)) unit = 10    
        end subroutine add_log_file
end module
    
program class_15
    use class_15_mod
    type(logger_type) :: logger
    integer :: unit
    character(len=100) :: filename
    call logger % add_log_file(unit=unit) !> which is already done
    call logger % add_log_file(filename=filename)
    call logger % add_log_file(unit=unit, filename=filename)
    call logger % add_log_file(filename=filename, unit=unit) ! reverse the order
    call logger % add_log_file()
    print *, filename
    if (filename /= "lfortran") error stop
    print *, unit
    if (unit /= 10) error stop
end program