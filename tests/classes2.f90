! Source - Page 44 of https://personalpages.manchester.ac.uk/staff/david.d.apsley/lectures/fortran/fortranB.pdf
module Defs
   implicit none
   private
   public point, point2d

    type, abstract :: point
        contains
        procedure(func), deferred :: radius
    end type point

    abstract interface
        real function func( this )
            import point
            class(point) this
        end function func
    end interface

    type, extends(point) :: point2d
        real x, y
    contains
        procedure :: radius => r2d
    end type point2d

contains

    real function r2d( this )
        class(point2d) this
        r2d = sqrt( this%x ** 2 + this%y ** 2 )
    end function r2d

end module Defs

program main
use Defs
implicit none

    real :: res
    class(point), pointer :: ptr

    type(point2d), target :: p2d = point2d( 3, 4 )

    ptr => p2d
    ! res = ptr%radius()
end program main
