module class_12_mod
    type, abstract :: abstract_type
    contains
       procedure(integer_method), deferred, nopass :: integer_method
    end type abstract_type

    abstract interface
       function integer_method() result(n)
          integer :: n
       end function integer_method
    end interface

    type :: Wrapper
       class(abstract_type),  allocatable :: obj
    contains
       procedure :: caller
    end type Wrapper

 contains

    subroutine caller(self)
       class(Wrapper), intent(in)  :: self
       integer :: n
       n = self%obj%integer_method()
    end subroutine caller
end module

program class_12
   use class_12_mod
   implicit none
   print *, "Ok"
end program
