! ! Check that lcompilers_get_i() returns mutated value of i not just hard coded value of 4
program arrays_52
    implicit none
    integer(4), dimension(11) :: cs
    integer(4), save :: nx = 4
 
    cs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    call test_1(cs)
    nx = 10
    call test_2(cs)
    
    contains
    
       subroutine test_1(x)
          integer(4), dimension(nx), intent(in) :: x
          print *, "Size of array x : ", size(x)
          if (size(x) /= 4) error stop
       end subroutine test_1
 
       subroutine test_2(x)
          integer(4), dimension(nx), intent(in) :: x
          print *, "Size of array x : ", size(x)
          if (size(x) /= 10) error stop
       end subroutine test_2
 
 end program arrays_52
