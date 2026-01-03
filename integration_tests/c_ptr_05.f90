program c_ptr_05
    use iso_c_binding, only: c_ptr, c_f_pointer, c_char, c_loc, c_null_char, c_associated
    implicit none
    type(c_ptr) :: plapla
    integer(8)  :: ii
    character(len=:,kind=c_char), allocatable, target :: buffer
    character(len=:), allocatable :: result
    ii = 4 
    allocate(character(len=ii+1,kind=c_char) :: buffer)
    buffer = c_char_"ABCD" // c_null_char
    plapla = c_loc(buffer)
    result = f_string_cptr_n(plapla, ii)
    print *, "Returned string:", trim(result)
    if (trim(result) /= "ABCD") error stop

contains

    function f_string_cptr_n(cptr, n) result(s)
        use iso_c_binding, only: c_ptr, c_f_pointer, c_char
        implicit none

        type(c_ptr), intent(in), value :: cptr
        integer(8),  intent(in)        :: n
        character(len=n,kind=c_char)   :: s
        character(len=n,kind=c_char), pointer :: sptr

        if (.not. c_associated(cptr)) then
            error stop "ERROR: f_string_cptr_n(): NULL C pointer passed"
        end if
        call c_f_pointer(cptr, sptr)
        if (.not. associated(sptr)) then
            error stop "ERROR: c_f_pointer(): failed to associate Fortran pointer"
        end if
        s = sptr
    end function f_string_cptr_n

end program 
