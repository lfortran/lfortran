module external_14_mod_2
    use iso_c_binding
    use external_14_mod_1, only : f_string
    implicit none

contains

    function get_ptr() result(r)
        type(c_ptr) :: r
        character(kind=c_char), target, save :: buf(6)

        buf = [ 'h','e','l','l','o', c_null_char ]
        r = c_loc(buf)
    end function get_ptr

    subroutine test()
        character(:), allocatable :: s

        s = f_string(get_ptr())

        print *, s
        if (s /= "hello") then
            error stop "Wrong string returned"
        end if
    end subroutine test

end module external_14_mod_2


program external_14
    use external_14_mod_2
    implicit none
    call test()
end program external_14
