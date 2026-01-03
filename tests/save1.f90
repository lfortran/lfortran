subroutine save_sub()
    real :: x = 5, z
    real, save :: y = 5
end subroutine

real function save_fun()
    implicit none
    real :: x = 5, z
    real, save :: y = 5
end function

module save_module
    implicit none
    public
    real :: x = 5, z
    real, save :: y = 5
    contains

    subroutine sub_save()
        real :: x = 5, z
        real, save :: y = 5
    end subroutine
    
    real function fun_save()
        implicit none
        real :: x = 5, z
        real, save :: y = 5
    end function

end module

program main
    real :: x = 5, z
    real, save :: y = 5
end program
