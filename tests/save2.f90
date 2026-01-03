subroutine save_sub()
    real :: x = 5, z
    real, save :: y = 5
    real :: a(5), b(5,4)
    save a, b
end subroutine

real function save_fun()
    implicit none
    real :: x = 5, z
    real, save :: y = 5
    real :: a(5), b(5,4)
    save a, b
end function

module save_module_2
    implicit none
    public
    real :: x = 5, z
    real, save :: y = 5
    real :: a(5), b(5,4)
    save a, b
    contains

    subroutine sub_save()
        real :: x = 5, z
        real, save :: y = 5
        real :: a(5), b(5,4)
        save a, b
    end subroutine
    
    real function fun_save()
        implicit none
        real :: x = 5, z
        real, save :: y = 5
        real :: a(5), b(5,4)
        save a, b
    end function

end module

program main
    real :: x = 5, z
    real, save :: y = 5
    real :: a(5), b(5,4)
    save a, b
end program
