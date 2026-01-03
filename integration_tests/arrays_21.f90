program arrays_21
   integer :: i6(6) = (/-14, 3, 0, -2, 19, 1/)
   integer :: i23(2, 3) = reshape((/-14, 3, 0, -2, 19, 1/), (/2, 3/))
   print *, i23                     ! writes -14   3
                                    !          0  -2
                                    !         19   1
   print *, maxloc(i6)              ! writes 5
   print *, maxloc(i23)             ! writes 1  3
   print *, maxloc(i23, dim=1)      ! writes 2  1  1
   print *, maxloc(i23, dim=2)      ! writes 3  1
   print *, maxloc(i23, dim=1, mask=(i23 < 10))
                                    ! writes 2  1  2
end program arrays_21
