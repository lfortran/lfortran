program data_struct
 implicit none
 type :: t
   integer :: a
 end type
 type(t) :: x, arr(2)
 data x /t(1)/
 data arr /t(2), t(3)/
 if (x%a /= 1 .or. arr(1)%a /= 2 .or. arr(2)%a /= 3) error stop
 print *, x%a, arr%a
end program
