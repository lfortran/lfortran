program gpu_metal_45
implicit none
integer :: x(5), y(4), i
x = 0
y = 0
do concurrent (i = 1:5)
  block
    block
      x(i) = i * 3
    end block
  end block
end do
if (x(1) /= 3) error stop
if (x(2) /= 6) error stop
if (x(3) /= 9) error stop
if (x(4) /= 12) error stop
if (x(5) /= 15) error stop
do concurrent (i = 1:4)
  block
    block
      block
        y(i) = i + 10
      end block
    end block
  end block
end do
if (y(1) /= 11) error stop
if (y(2) /= 12) error stop
if (y(3) /= 13) error stop
if (y(4) /= 14) error stop
print *, "ok"
end program
