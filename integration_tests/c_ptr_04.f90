module mod_uop
    contains
    subroutine uop( sendbuf)
        use iso_c_binding, only: c_loc, c_ptr, c_null_ptr, c_associated
        real(8), dimension(:), intent(in), target :: sendbuf
        type(c_ptr) :: c_send
        c_send = c_null_ptr
        c_send = c_loc(sendbuf)
        
        print *, 'C pointer: ', c_send

        if (.not. c_associated(c_send)) then
            error stop 'Error: c_send is null'
        end if
    end subroutine
end module mod_uop

program c_ptr_04
    use mod_uop
    use iso_c_binding, only: c_loc,c_ptr
    real(8)  :: vin(65000)
    ! type(c_ptr) :: c_send
    call uop(vin)
    ! c_send = c_loc(vin)
    ! print *, 'C pointer: ', c_send
end program c_ptr_04