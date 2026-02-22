! Test extends_type_of with class(*)
program class_118
   implicit none
   type :: animal
      integer :: legs
   end type animal

   type, extends(animal) :: dog
      character(len=20) :: breed
   end type dog

   type :: plant
      integer :: leaves
   end type plant

   class(*), allocatable :: a, b, c, d

   a = dog(legs=4, breed="poodle")
   b = animal(legs=2)
   c = dog(legs=4, breed="labrador")
   d = plant(leaves=5)

   ! dog extends animal
   if (.not. extends_type_of(a, b)) error stop
   ! animal does not extend dog
   if (extends_type_of(b, a)) error stop
   ! dog extends dog (same type)
   if (.not. extends_type_of(a, c)) error stop
   ! animal extends animal (same type)
   if (.not. extends_type_of(b, b)) error stop
   ! dog does not extend plant (unrelated)
   if (extends_type_of(a, d)) error stop
   ! plant does not extend animal (unrelated)
   if (extends_type_of(d, b)) error stop

   print *, "PASS"
end program class_118
