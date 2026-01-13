program intrinsics_405
    implicit none
    call print_any("  Hello World  ")
    contains
    subroutine print_any(generic)
        class(*), intent(in) :: generic
        character(len = 15) :: a, b, c 
        integer :: d,e
        select type (generic)
        type is (character(len=*))

            a = trim(generic)
            print *, trim(generic)
            if (trim(generic) /= "  Hello World  ") error stop

            b = adjustl(generic)
            print *, adjustl(generic)
            if (adjustl(generic) /= "Hello World  ") error stop

            c = adjustr(generic)
            print *, adjustr(generic)
            if (adjustr(generic) /= "    Hello World") error stop

            d = len_trim(generic)
            print *, len_trim(generic)
            if (len_trim(generic) /= 13) error stop
            
            e = len(generic)            
            print *, len(generic)
            if (len(generic) /= 15) error stop

        end select

        print *, a
        if (a /= "  Hello World  ") error stop
        print *, b
        if (b /= "Hello World  ") error stop 
        print *, c 
        if (c /= "    Hello World") error stop
        print *, d
        if (d /= 13) error stop
        print *, e
        if (e /= 15) error stop
        
    end subroutine print_any

end program intrinsics_405