! An external procedure declared in a module interface block and defined in a
! separate file must receive the CHARACTER descriptor, not its raw data pointer.
module netcdf_nf_interfaces_52
    interface
        function nf_get_vara_text(text) result(status)
            character(len=*), intent(out) :: text
            integer :: status
        end function nf_get_vara_text
    end interface
end module netcdf_nf_interfaces_52

program implicit_interface_55
    use netcdf_nf_interfaces_52
    implicit none
    character(len=14) :: s
    integer :: r
    r = nf_get_vara_text(s)
    print *, "LEN(text) seen by callee =", r, " (expected 14)"
    if (r /= 14) error stop
    print *, "OK"
end program implicit_interface_55
