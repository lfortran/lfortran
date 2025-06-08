! Overloading the assignment operator `=` with a function call that would be handled by
! `subroutine_from_function` pass. So we need to make sure that the pass 
! actually replaces the function call in the overloaded expression.
module operator_overloading_07_mod1
    implicit none
    interface assignment(=)
        module procedure :: assign_string_char
    end interface assignment(=)

    type string_type
        character(:), allocatable :: raw
    end type

    contains

    subroutine assign_string_char(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_string_char

end module operator_overloading_07_mod1


module operator_overloading_07_mod2
    use operator_overloading_07_mod1
    implicit none

    type(string_type) :: instance
    
    contains
    function func() result(ret_arr)
        character(:), allocatable :: ret_arr
        allocate(character(15) :: ret_arr)
        ret_arr = "Hello World!"
    end function

    subroutine ff()
        character(:), allocatable :: inp
        instance = func() ! pass `subroutine_from_function` takes action here. It must handle the overloading of `=`. 
        print *, instance%raw
        if(instance%raw /= "Hello World!") error stop
    end subroutine ff
end module


program operator_overloading_07
    use operator_overloading_07_mod2, only : ff
    implicit none
    call ff()
end program  