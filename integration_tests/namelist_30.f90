program namelist_29
   implicit none
   character(len=:), allocatable :: type
   character(len=100) :: buffer1, buffer2

   namelist /args/ type, /long/ type

   allocate(character(len=100) :: type)

! automatic input for args
   buffer1 = "&args type='value_from_args' /"
   read(buffer1, nml=args)
   print *, "After reading args:", trim(type)
   if (trim(type) /= "value_from_args") error stop

! automatic input for long
   buffer2 = "&long type='value_from_long' /"
   read(buffer2, nml=long)
   print *, "After reading long:", trim(type)
   if (trim(type) /= "value_from_long") error stop

end program namelist_29
