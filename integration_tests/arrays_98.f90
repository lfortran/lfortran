module arrays_98_mod

    character(*), parameter :: hello = "HELLO"
    character(*), parameter :: bye = "BYE"
 
    contains
 
    logical function ff(lhs)
       character(*), dimension(:), intent(in)  :: lhs
       ff =  ff2(lhs) 
    end function ff
 
    logical function ff2(lhs)
    character(len=*), dimension(:), intent(in)  :: lhs
    ff2 = all(lhs==hello)
    end function ff2
 end module 
 
 
 program arrays_98
    use arrays_98_mod
    character(5) :: arr(20)
    arr = hello
    call ss(arr,.true.)
 
    arr = bye
    call ss(arr, .false.)
 
    contains 
    subroutine ss(s, expected)
       character(*) :: s(:)
       logical :: expected
       if ( expected .neqv. ff(arr) ) error stop
    end subroutine
 
 
 end program
 