subroutine test()
    implicit none
    
    integer :: i1
    common /blocki/ i1
    
    character(5) :: c1
    common /blockc/ c1
    
    if (i1 /= 12345) error stop
    if (c1 /= '12345') error stop
    
end subroutine

program block_data_string_01
implicit none

call test()

end program

block data bd
implicit none

integer :: i1
common /blocki/ i1
data i1 /12345/

character(5) :: c1
common /blockc/ c1
data c1 /'12345'/

end block data
