program namelist_01
   implicit none

   integer :: count = 10
   integer :: max_iter = 100
   real :: tolerance = 1.0e-6
   real :: factor = 1.5
   logical :: verbose = .false.
   character(len=30) :: method = 'default'

   namelist /config/ count, max_iter, tolerance, factor, verbose, method
end program namelist_01
