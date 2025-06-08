program arrays_85
    use iso_fortran_env, only: int8
    integer(int8), target :: full_key(1:4, 1:3)
    integer(int8), pointer :: full_key_ptr(:, :)

    full_key = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [4, 3])
    if( lbound(full_key, 1) /= 1 ) error stop
    if( any(full_key /= reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [4, 3])) ) error stop

    full_key_ptr => full_key
    call process_key(full_key_ptr)

    print *, full_key
    if( any(full_key /= reshape([1, 2, 2, 4, 5, 6, 99, 8, 9, 10, 11, 12], [4, 3]) ) ) error stop

    call process_key(full_key(2:4, 1:2))
    print *, full_key
    if( any(full_key /= reshape([1, 2, 2, 2, 5, 6, 99, 99, 9, 10, 11, 12], [4, 3]) ) ) error stop
contains

    subroutine process_key(key)
        integer(int8), intent(inout) :: key(0:, :)
        if( lbound(key, 1) /= 0 ) error stop
        key(2, 1) = 2
        key(2, 2) = 99
    end subroutine process_key

end program arrays_85
