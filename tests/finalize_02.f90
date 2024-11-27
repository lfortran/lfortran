program deallocate_01
    implicit none
 
    integer, allocatable :: my_int(:)
 
    type my_type
       integer, allocatable :: int(:)
    end type
 
    type(my_type) :: x
 
    allocate(x%int(2))
    allocate(my_int(2))
 
    call sub
    call sub
 
 contains
 
    subroutine sub
       type my_type_sub
          integer, allocatable :: int(:)
       end type
 
       type(my_type) :: x_sub
 
       allocate(x_sub%int(2))
    end subroutine sub
 end program
 