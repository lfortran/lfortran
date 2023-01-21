module debug_2
    implicit none
    private
    public :: t_2

    type, abstract :: t_2
    contains
      procedure(sub), deferred :: sub
    end type t_2

    abstract interface
      subroutine sub(arg)
         import :: t_2
         class(t_2), intent(inout) :: arg
      end subroutine sub
   end interface

end module debug_2
