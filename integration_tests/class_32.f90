module class_32_test_module
   implicit none

   type :: Composed
      integer :: x
   end type Composed

   type :: Base
      class(Composed), allocatable :: obj
   end type

   type :: Super
      integer :: x
   end type Super

   type, extends(Super) :: Derived
      integer :: y
   end type

end module class_32_test_module

program class_32
   use class_32_test_module
   implicit none

   class(Composed), allocatable :: c
   class(Composed), allocatable :: d

   class(Base), allocatable :: c_base
   class(Base), allocatable :: d_base

   class(Super), allocatable :: c_super
   class(Derived), allocatable :: d_derived


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

   ! test case 3: assignment of derived class var to base class var
   allocate(c_super)
   c_super%x = 1

   print *, "c_super%x: ", c_super%x

   allocate(d_derived)
   d_derived%x = 42

   print *, "d_derived%x: ", d_derived%x

   c_super = d_derived
   print *, "c_super%x after assignment: ", c_super%x

   d_derived%x = 2
   ! verify deep copy
   print *, "c_super%x after assignment: ", c_super%x
   if (c_super%x == 2) error stop

end program class_32