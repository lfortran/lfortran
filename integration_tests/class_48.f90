program class_48
   implicit none

   type :: SeriesInt
      integer :: scalar_data
      integer, allocatable :: array_data(:)
   end type

   type :: SeriesIntNested
      type(SeriesInt), allocatable :: v
   end type

   interface Series
      procedure Series_scalar, Series_array
   end interface

   interface SeriesNested
      procedure Series_nested_scalar_int
   end interface

   class(SeriesInt), allocatable :: s1, s2
   class(SeriesIntNested), allocatable :: s3

   ! Call Series with integer
   s1 = Series(42)
   print *, "s1%scalar_data:", s1%scalar_data
   print *, "s1%array_data:", s1%array_data
   if (s1%scalar_data /= 42) error stop
   if (.not. all(s1%array_data == [0, 0, 0, 0])) error stop

   print *

   ! Call Series with array
   s2 = Series([1,2,3,4])
   print *, "s2%scalar_data:", s2%scalar_data
   print *, "s2%array_data:", s2%array_data
   if (s2%scalar_data /= 101) error stop
   if (.not. all(s2%array_data == [1, 2, 3, 4])) error stop

   print *

   allocate(s3)

   ! Call SeriesNested with integer and assign to a struct instance member
   s3%v = SeriesNested(42)
   print *, "s3%v%scalar_data:", s3%v%scalar_data
   if (s3%v%scalar_data /= 42) error stop

contains

   function Series_scalar(data) result(self)
      type(SeriesInt) :: self
      integer, intent(in) :: data

      self%scalar_data = data
      allocate(self%array_data(4))  ! initialize to zeroes
   end function

   function Series_nested_scalar_int(data) result(self)
      type(SeriesInt) :: self
      integer, intent(in) :: data

      self%scalar_data = data
   end function

   function Series_array(data) result(self)
      type(SeriesInt) :: self
      integer, intent(in) :: data(:)

      self%array_data = data
      self%scalar_data = 101
   end function

end program class_48
