module pass_array_by_data_09_mod
    interface interface_1
    procedure :: ff
    end interface interface_1
    contains 
    function ff(inp) result(oout)
      integer, intent(in) :: inp(:)
      integer :: oout
      print *, inp
      if(any(inp /= [1,2,3,4,5])) error stop 
      oout = 11
    end function
  end module pass_array_by_data_09_mod
  
  
  
  
  program pass_array_by_data_09
    use pass_array_by_data_09_mod
    
    type :: tt
      integer :: i
    end type tt

    integer, allocatable :: ll(:)
    type(tt) :: inst_tt
    call ffo(inst_tt)

    contains

    subroutine ffo(dummy) 
        class(tt), intent(in):: dummy ! This shouldn't idenify this function `ffo` as classProcedure. It's a normal function.
        integer :: r
        integer, dimension(5) :: rr
        rr = [1,2,3,4,5]
        r = interface_1(rr)
        print *, r
        if(r /= 11) error stop
    end subroutine ffo
  end program pass_array_by_data_09