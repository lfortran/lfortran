module nested_17_mod
    type t
        integer :: i = 1
    end type

    type(t), parameter :: t_param = t() 
    contains
    
    subroutine ss()
        type(t), parameter :: t_param2 = t() 
        call ss_in
        contains
        subroutine ss_in()
            print *, t_param%i
            if(t_param%i /= 1) error stop

            print *, t_param2%i
            if(t_param2%i /= 1) error stop
        end subroutine

    end subroutine
end module 

program nested_17
    use nested_17_mod
    call ss()
end program