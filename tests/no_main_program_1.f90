module no_main_program_1_mod
    implicit none
contains
    function add(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function add
end module no_main_program_1_mod
