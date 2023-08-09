program arrays_30
implicit none
   integer    :: i1(64) = [  479,  735, -870,  410, 446, -627,  879,  -72, &
                            -948,  693,  133, -672, 897, -227,  965,    9, &
                               0,  997,  721, -240, 765, -426,  361,  751, &
                             669,  -94,  534,  806, -23,  844,  246, -114, &
                             673, -445,  654, -274, -53,  505, -414,  269, &
                            -345,  268, -544,  629, 353, -808,  199,  322, &
                             995, -697,  290, -514, 350, -777,  664,  195, &
                            -401,  348,   86, -620, 153,  144,   65, -431  ]
   integer    :: i2(4, 16)
   integer    :: i3(2, 4, 8)
   integer    :: i4(2, 4, 2, 4)
   integer    :: res_01(1), res_02(2), res_03(3), res_04(4)
   integer    :: max = 997

   i2 = reshape(i1, [4, 16])
   i3 = reshape(i1, [2, 4, 8])
   i4 = reshape(i1, [2, 4, 2, 4])

   res_01 = maxloc(i1)
   res_02 = maxloc(i2)
   res_03 = maxloc(i3)
   res_04 = maxloc(i4)

   if (maxloc(i1, 1) /= 18) error stop
   if (i1(res_01(1)) /= max) error stop
   if (i2(res_02(1), res_02(2)) /= i1(res_01(1))) error stop
   if (i3(res_03(1), res_03(2), res_03(3)) /= max) error stop
   if (i4(res_04(1), res_04(2), res_04(3), res_04(4)) /= max) error stop

   res_01 = shape(maxloc(i1))
   if (res_01(1) /= 1) error stop
   res_01 = shape(maxloc(i2, 1))
   if (res_01(1) /= 16) error stop
   res_02 = shape(maxloc(i3, 2))
   if (res_02(1) /= 2 .and. res_02(2) /= 8) error stop
   res_03 = shape(maxloc(i4, 3))
   if (res_03(1) /= 2 .and. res_03(2) /= 8 .and. res_03(3) /= 4) error stop

   res_04 = maxloc(i2, 2)
   if (i2(1, res_04(1)) /= 995) error stop
   if (i2(2, res_04(2)) /= max) error stop
   if (i2(3, res_04(3)) /= 965) error stop
   if (i2(4, res_04(4)) /= 806) error stop
end program arrays_30
