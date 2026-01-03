subroutine idd_random_transf_init(value)
    implicit none
    real(8), intent(in) :: value
    save
    if( value /= 42.0d0) error stop
    return
end

program save_06
    implicit none
    real(8) :: initialValue
    initialValue = 42.0d0
    call idd_random_transf_init(initialValue)
end program
