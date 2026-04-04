! Test: do concurrent inside block inside associate block.
! The gpu_offload pass must resolve associate variables referenced
! as loop bounds even when the do concurrent is nested inside
! a block that is itself inside an associate construct.
program gpu_metal_41
implicit none
integer :: n, l, a(3)
n = 3
a = 0
associate(m => n)
  block
    do concurrent(l = 1:m)
      a(l) = l * 10
    end do
  end block
end associate
if (a(1) /= 10) error stop
if (a(2) /= 20) error stop
if (a(3) /= 30) error stop
end program
