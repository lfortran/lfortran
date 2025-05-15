program test_key_slice
    use iso_fortran_env, only: int8
    integer(int8), dimension(10) :: full_key
    integer :: i
    call process_key(full_key(:))
contains

    subroutine process_key(key)
        integer(int8), intent(in) :: key(0:)
        integer :: i
        print *, lbound(key, 1)
        if( lbound(key, 1) /= 0 ) error stop
    end subroutine process_key

end program test_key_slice
