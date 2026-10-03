module bindc_13_mod
use iso_c_binding, only: c_char
implicit none

interface
    pure subroutine caf_fatal_error(str) bind(C)
    import :: c_char
    character(kind=c_char,len=:), pointer, intent(in) :: str
    end subroutine

    subroutine process_str(str) bind(C)
    import :: c_char
    character(kind=c_char,len=*), intent(in) :: str
    end subroutine
end interface

end module

program bindc_13
use bindc_13_mod
implicit none
print *, "ok"
end program
