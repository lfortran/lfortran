! Test: associate variable used inside a block within do concurrent.
! The gpu_offload pass must resolve associate aliases in both block
! body statements and block-local type expressions before kernel
! extraction, because the kernel scope cannot access the
! AssociateBlock's symbol table.
program gpu_metal_44
implicit none
integer :: x, l, res(3)
x = 3
res = 0
associate(n => x)
  do concurrent (l = 1:n)
    block
      integer :: tmp
      tmp = n + l
      res(l) = tmp
    end block
  end do
end associate
if (res(1) /= 4) error stop
if (res(2) /= 5) error stop
if (res(3) /= 6) error stop
print *, "ok"
end program
