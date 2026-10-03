program pointer_16
  implicit none
  class(*), pointer :: ptr
  
  call ss(ptr)
  contains 
  subroutine ss(xx)
    class(*), pointer :: xx
    character(len=10), pointer :: ii
    allocate(ii)
    xx => ii
    deallocate(ii)
    
  end subroutine ss
end program 