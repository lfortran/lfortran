program array_reshape_01
   implicit none

   integer :: box(3,4) =reshape([  &
      1, 4, 7, 10, &
      2, 5, 8, 11, &
      3, 6, 9, 12  &
      ],shape=shape(box),order=[2,1])

   integer,allocatable :: v(:,:)
   integer             :: rc(2)
   integer             :: expected_v1(8,6)
   integer             :: expected_v2(8,6)

   ! making the result bigger than source using pad
   rc(2:1:-1)=shape(box)

   expected_v1 = reshape([1, 7, -1, -1, -1, -1, -1, -1, 2, 8, -2, -2, -2, -2, -2, &
      -2, 3, 9, -3, -3, -3, -3, -3, -3, 4, 10, -1, -1, -1, -1, &
      -1, -1, 5, 11, -2, -2, -2, -2, -2, -2, 6, 12, -3, -3, -3, &
      -3, -3, -3], [8,6])
   expected_v2 = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, &
      -1, -2, -3, -1, -2, -3, -1, -2, -3, -1, -2, -3, -1, -2, -3, &
      -1, -2, -3, -1, -2, -3, -1, -2, -3, -1, -2, -3, -1, -2, -3, &
      -1, -2, -3], [8,6])

   ! CASE I
   ! if flatten box to a vector it runs with wrong result
   v=reshape([box],rc*2,pad=[-1,-2,-3],order=[2,1])
   if (any(v /= expected_v1)) error stop 1

   ! CASE II
   ! simple vector produces wrong result with no order= option
   v=reshape([1,2,3,4,5,6,7,8,9,10,11,12],rc*2,pad=[-1,-2,-3])
   if (any(v /= expected_v2)) error stop 2

   ! CASE III
   ! simple vector produces segfault with order= specified
   v=reshape([1,2,3,4,5,6,7,8,9,10,11,12],rc*2,pad=[-1,-2,-3],order=[2,1])
   if (any(v /= expected_v1)) error stop 3

   ! CASE IV
   ! compiler error
   v=reshape(pack(box,.true.),rc*2,pad=[-1,-2,-3],order=[2,1])
   if (any(v /= expected_v1)) error stop 4

   ! CASE V
   ! this produces compiler error when box is a matrix and pad is a vector
   v=reshape(box,rc*2,pad=[-1,-2,-3],order=[2,1])
   if (any(v /= expected_v1)) error stop 5

   print *, "All tests passed!"

end program array_reshape_01
