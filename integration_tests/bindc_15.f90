! Test c_f_pointer for character pointer with expression length
program bindc_15
    use iso_c_binding
    implicit none

    character(:), allocatable :: s
    character(12, kind=c_char), target, save :: buf
    type(c_ptr) :: cptr

    buf = "hello world" // c_null_char
    cptr = c_loc(buf)
    s = f_string_pointer(cptr)

    if (len(s) /= 11) error stop
    if (s /= "hello world") error stop
    print *, "PASS"

contains

    function f_string_pointer(cptr) result(fptr)
        type(c_ptr), intent(in) :: cptr
        character(:), pointer :: fptr
        interface
            function strlen(s) result(len) bind(c)
                use iso_c_binding
                type(c_ptr), value :: s
                integer(c_size_t) :: len
            end function
        end interface
        fptr => f_string_pointer_aux(cptr, strlen(cptr))
    contains
        function f_string_pointer_aux(cptr, len) result(fptr)
            type(c_ptr), intent(in) :: cptr
            integer(c_size_t), intent(in) :: len
            character(len), pointer :: fptr
            call c_f_pointer(cptr, fptr)
        end function
    end function

end program
