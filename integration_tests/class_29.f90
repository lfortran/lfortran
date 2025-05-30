module class_29_test_module
   implicit none

   type :: Composed
      integer :: x
   end type Composed

   type :: Base
      class(Composed), allocatable :: obj
   end type

end module class_29_test_module

program class_29
   use class_29_test_module
   implicit none

   class(Composed), allocatable :: c
   class(Composed), allocatable :: d

   class(Base), allocatable :: c_base
   class(Base), allocatable :: d_base

   ! test case 1: assignment of class var to class var
   allocate(c)
   c%x = 1

   print *, "c%x: ", c%x

   allocate(d)
   d%x = 42

   print *, "d%x: ", d%x

   c = d
   print *, "c%x after assignment: ", c%x
   if (c%x /= 42) error stop


   d%x = 3
   print *, "d%x: ", d%x

   ! verify deep copy
   print *, "c%x: ", c%x
   if (c%x == 3) error stop

   ! test case 2: assignment of class var to struct member class
   allocate(c_base)
   allocate(d_base)

   allocate(c_base%obj) ! needed because lfortran does not automatically allocate this
   allocate(d_base%obj) ! needed because lfortran does not automatically allocate this

   c_base%obj = c
   d_base%obj = d

   print *, "c_base%obj%x: ", c_base%obj%x
   if (c_base%obj%x /= 42) error stop

   print *, "d_base%obj%x: ", d_base%obj%x
   if (d_base%obj%x /= 3) error stop


   c%x = 20
   print *, "c%x: ", c%x
   
   ! verify deep copy
   print *, "c_base%obj%x: ", c_base%obj%x
   if (c_base%obj%x == 20) error stop

end program class_29
