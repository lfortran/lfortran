! The module, its submodule and the program are in one file, so the
! submodule must be taken from this translation unit. The include
! directory holds a foreign file named like the submodule's .smod
! (submodule_same_file_smod_01_m@submodule_same_file_smod_01_s.smod),
! which must not be read.
module submodule_same_file_smod_01_m
    implicit none
    interface
        module function twice(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function
        module subroutine add_one(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
end module

submodule (submodule_same_file_smod_01_m) submodule_same_file_smod_01_s
    implicit none
contains
    module function twice(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = 2*x
    end function

    module subroutine add_one(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine
end submodule

program submodule_same_file_smod_01
    use submodule_same_file_smod_01_m
    implicit none
    integer :: i
    i = twice(21)
    if (i /= 42) error stop
    call add_one(i)
    if (i /= 43) error stop
    print *, i
end program
