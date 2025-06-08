module subroutines_15_module
contains
    subroutine cb(a,x)
        implicit none
        integer, intent(in) :: a 
        integer, intent(in), optional :: x(:)
    end subroutine cb
end module subroutines_15_module

program subroutines_15
    use subroutines_15_module
    call temp(cb)
contains
    subroutine temp(call_back)
        implicit none
        procedure(cb), optional :: call_back
        integer :: a
        call call_back(a)
    end subroutine temp
end program subroutines_15
