module data_24_mod
   implicit none
   integer, save :: in_module(4)
   integer, save :: colormap(3, 0:4)
   data in_module(1:2) / 100, 200 /, in_module(3:4) / 300, 400 /
   data colormap(1:3, 0:2) / 11, 12, 13, 21, 22, 23, 31, 32, 33 /, &
        colormap(1:3, 3:4) / 6*77 /
end module data_24_mod

program data_24
   use data_24_mod
   implicit none
   integer :: in_main(4)
   integer :: i
   data in_main(1:2) / 100, 200 /, in_main(3:4) / 300, 400 /

   do i = 1, 4
      if (in_main(i) /= i * 100) error stop
      if (in_module(i) /= i * 100) error stop
   end do
   do i = 1, 3
      if (colormap(i, 0) /= 10 + i) error stop
      if (colormap(i, 1) /= 20 + i) error stop
      if (colormap(i, 2) /= 30 + i) error stop
      if (colormap(i, 3) /= 77) error stop
      if (colormap(i, 4) /= 77) error stop
   end do
   print *, "ok"
end program data_24
