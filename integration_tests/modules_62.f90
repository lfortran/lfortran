module modules_61_module
    implicit none
    private

contains 
    
    subroutine get_other( other, value )
        integer, intent(in) :: other
        integer, intent(out) :: value
        error stop
    end subroutine get_other
end module 

program test_modules_61
    use modules_61_module
    implicit none
    integer :: map, key
    logical :: exists

    map = 91
    key = 12
    call get_other( map, key, exists )
    print *, "exists = ", exists
    if ( .not. exists ) error stop

contains 
    subroutine get_other( map, key, exists )
        integer, intent(inout) :: map
        integer, intent(in)         :: key
        logical, intent(out) :: exists
        print *, "get_other called with map = ", map, " and key = ", key
        if ( map /= 91 ) error stop
        if ( key /= 12 ) error stop
        exists = .true.
    end subroutine get_other
end program
