program empty_array_02
   integer, parameter :: z(0) = 0

   if (any(z > 0)) error stop
end program
