program gpu_metal_73
! Test: all() with array slice inside do concurrent inside associate block
! compiled with --gpu=metal. Previously caused ASR verify failure because
! inline_intrinsic_all placed temporaries in AssociateBlock scope.
implicit none
integer :: l
logical :: a(3), res(3)

a = .true.
a(2) = .false.

associate(n => 3)
  do concurrent(l = 1:n)
    res(l) = all(a(1:l))
  end do
end associate

! a(1:1) = [.true.]       -> all = .true.
! a(1:2) = [.true.,.false.] -> all = .false.
! a(1:3) = [.true.,.false.,.true.] -> all = .false.
if (res(1) .neqv. .true.)  error stop
if (res(2) .neqv. .false.) error stop
if (res(3) .neqv. .false.) error stop
print *, "PASS"
end program
