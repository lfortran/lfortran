program implied_do_loop10
    implicit none
    integer :: i
    type :: col
       integer :: rgb(3)
    end type col
    type(col) :: colours
    data (colours%rgb(i), i=1, 3) /1, 2, 3/
    print *, "colours%rgb : ", colours%rgb
    if (any(colours%rgb /= [1, 2, 3])) error stop
end program
