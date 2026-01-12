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

            b = adjustl(generic)
            print *, adjustl(generic)

            c = adjustr(generic)
            print *, adjustr(generic)

            d = len_trim(generic)
            print *, len_trim(generic)
            
            e = len(generic)            
            print *, len(generic)

        end select

        print *, a
        if (a /= trim("  Hello World  ")) error stop
        print *, b
        if (b /= adjustl("  Hello World  ")) error stop 
        print *, c 
        if (c /= adjustr("  Hello World  ")) error stop
        print *, d
        if (d /= len_trim("  Hello World  ")) error stop
        print * ,e
        if (e /= len("  Hello World  ")) error stop
        
    end subroutine print_any

end program intrinsics_405