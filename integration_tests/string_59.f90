module string_59_mod

    contains 
    subroutine ss(i)
      integer :: i
      character(len=3), save:: nan_string='NaN'
      print *, nan_string

      ! Checks
      if(i == 1) then
        if( nan_string /= "123") error stop
      else if ( i == 2 ) then
        if( nan_string /= "456") error stop
      end if

      ! Set
      if(i == 3) then
        nan_string = "123"
      else if (i == 4) then
        nan_string = "456"
      end if
    end subroutine
end module
  
program string_59
    use string_59_mod
    call ss(3)
    call ss(1)
    call ss(4)
    call ss(2)
end program