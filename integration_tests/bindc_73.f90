module bindc_73_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
contains
    function get_c() result(c) bind(c)
        character(kind=c_char) :: c
        c = 'q'
    end function

    integer function take_c(c) result(i)
        character, intent(in) :: c
        i = iachar(c)
    end function

    function outer_c() result(c) bind(c)
        character(kind=c_char) :: c
        c = inner_c()
    contains
        function inner_c() result(c)
            character(3) :: c
            c = 'qrs'
        end function
    end function
end module

program bindc_73
    use bindc_73_mod
    implicit none
    if (take_c(get_c()) /= iachar('q')) error stop 4
    if (outer_c() /= 'q') error stop 15
end program
