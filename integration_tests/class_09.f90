module class_09_module
    implicit none
    private
    public point, point2d, point3d

     type, abstract :: point
         contains
         procedure(func), deferred :: radius
     end type point

     abstract interface
         subroutine func( this )
             import point
             class(point) this
         end subroutine func
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

    subroutine r2d( this )
         class(point2d) this
         print *, sqrt( this%x ** 2 + this%y ** 2 )
     end subroutine r2d

     subroutine r3d( this )
         class(point3d) this
         print *, sqrt( this%x ** 2 + this%y ** 2 + this%z ** 2 )
     end subroutine r3d

 end module class_09_module

 program class_09_program
 use class_09_module
 implicit none

     class(point), pointer :: ptr

     type(point2d), target :: p2d = point2d( 3, 4 )
     type(point3d), target :: p3d = point3d( 3, 4, 5 )

     print *, p2d%x, p2d%y
     print *, p3d%x, p3d%y, p3d%z

     ptr => p2d

     print *, "2-d radius is: "
     call ptr%radius()

     ptr => p3d
     print *, "3-d radius is: "
     call ptr%radius()

     p3d%x = 3.0
     p3d%y = 4.0
     p3d%z = 0.0
     print *, "3-d radius is: "
     call ptr%radius()
 end program class_09_program
