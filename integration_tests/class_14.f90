module malloc_failure
   
    implicit none
    
    type, public, abstract :: AbsType
    contains
       procedure(callee), deferred, nopass :: callee
    end type AbsType
    
    abstract interface
       subroutine callee(mode)
          integer, intent(in)  :: mode
       end subroutine callee
    end interface
       
    type :: SomeClass
       private
       class(AbsType), allocatable :: object
    contains
       procedure :: caller
    end type SomeClass
    
    interface SomeClass
       procedure :: constructor
    end interface SomeClass
    
 contains
 
    function constructor(object) result(self)
       type(SomeClass)            :: self
       class(AbsType), intent(in) :: object
       self%object = object
    end function constructor
    
    subroutine caller(self)
       class(SomeClass), intent(in)  :: self
       call self%object%callee(mode=0)
    end subroutine caller
 
end module malloc_failure
program main
end program