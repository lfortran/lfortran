program intrinsics_456
   ! move_alloc must preserve the lower (and upper) bounds of the source
   ! allocatable array on the destination.
   implicit none
   real(8), allocatable :: a(:,:,:,:), b(:,:,:,:)
   allocate(a(-2:0, -2:0, 1:2, 1:2))
   a = 5.0d0
   call move_alloc(a, b)
   if (allocated(a)) error stop "a still alloc"
   if (.not. allocated(b)) error stop "b not alloc"
   if (lbound(b,1) /= -2) error stop "lb1"
   if (ubound(b,1) /=  0) error stop "ub1"
   if (lbound(b,2) /= -2) error stop "lb2"
   if (ubound(b,2) /=  0) error stop "ub2"
   if (lbound(b,3) /=  1) error stop "lb3"
   if (ubound(b,3) /=  2) error stop "ub3"
   if (lbound(b,4) /=  1) error stop "lb4"
   if (ubound(b,4) /=  2) error stop "ub4"
   if (abs(b(-2,-2,1,1) - 5.0d0) > 1.0d-12) error stop "v"
   deallocate(b)
   print *, "PASS"
end program
