module mod_derived_types_64
    implicit none
    
    ! derived type with the same name as interface
    type :: toml_datetime
        character(len=:), allocatable :: date
    end type toml_datetime

    ! same name is derived type above
    interface toml_datetime
        module procedure :: toml_datetime_ctor
    end interface

contains
    function toml_datetime_ctor(date_str, time_str) result(dt)
        character(len=*), intent(in) :: date_str, time_str
        type(toml_datetime) :: dt

        ! raise an error if this function is executed
        error stop
        print *, "This function isn't executed"
        dt%date = date_str
    end function toml_datetime_ctor
end module

program derived_types_64
    use mod_derived_types_64
    implicit none
    type(toml_datetime) :: ts1
    character(len=10) :: tmp = "2025-06-11"
    ts1 = toml_datetime(tmp)
    print *, "Date: ", ts1%date
    if (ts1%date /= "2025-06-11") error stop
end program derived_types_64
