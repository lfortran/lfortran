! Tests large string allocation (Bombs memory if not freed)
module string_81_mod
    implicit none
    contains
    subroutine ff()
      character(10000) :: ss
      character(10000) :: ss2
      ss(1:1) = "a" ! just to use the variable
      ss2(1:1) = "a" ! just to use the variable
    end subroutine
    
end module
  
program string_81
  use string_81_mod
  integer :: i
  do i =0 , 10000000
    call ff()
  end do

end program