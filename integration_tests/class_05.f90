! Source - Page 44 of https://personalpages.manchester.ac.uk/staff/david.d.apsley/lectures/fortran/fortranB.pdf
module Defs
   implicit none
   private
   public point, point2d, point3d

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

    type, extends(point2d) :: point3d
        real z
    contains
        procedure :: radius => r3d
    end type point3d

contains

    real function r2d( this )
        class(point2d) this
        r2d = sqrt( this%x ** 2 + this%y ** 2 )
    end function r2d

    real function r3d( this )
        class(point3d) this
        r3d = sqrt( this%x ** 2 + this%y ** 2 + this%z ** 2 )
    end function r3d

end module Defs

program main
use Defs
implicit none

    class(point), pointer :: ptr

    type(point2d), target :: p2d = point2d( 3, 4 )
    type(point3d), target :: p3d = point3d( 3, 4, 5 )

    real :: result

    print *, p2d%x, p2d%y
    print *, p3d%x, p3d%y, p3d%z

    ptr => p2d
    result = ptr%radius()
    print *, "2-d radius is ", result
    if( abs(result - 5.0) > 1e-8 ) error stop

    ptr => p3d
    result = ptr%radius()
    print *, "3-d radius is ", result
    if( abs(result - 7.07106781) > 1e-8 ) error stop

    p3d%x = 3.0
    p3d%y = 4.0
    p3d%z = 0.0
    result = ptr%radius()
    print *, "3-d radius is ", result
    if( abs(result - 5.0) > 1e-8 ) error stop
end program main
