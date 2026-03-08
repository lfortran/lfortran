! Test: associate block with array slices in a loop does not leak memory
! when --realloc-lhs-arrays is enabled.
program associate_39
  implicit none
  real, allocatable :: w(:,:,:), b(:,:), dcdw(:,:,:), dcdb(:,:)
  integer :: l, batch

  allocate(w(3,3,3), b(3,3), dcdw(3,3,3), dcdb(3,3))
  w = 0.; b = 0.; dcdb = 1.

  do batch = 1, 5000
    dcdw = 0.
    associate(x => 1.5)
      do l = 1, 3
        b(:,l) = b(:,l) - x*dcdb(:,l)
        dcdw(:,:,l) = dcdw(:,:,l)*1
        w(:,:,l) = w(:,:,l) - x*dcdw(:,:,l)
      end do
    end associate
  end do

  if (abs(b(1,1) - (-7500.0)) > 1e-2) error stop
  if (abs(b(2,3) - (-7500.0)) > 1e-2) error stop
  print *, "ok"
end program
