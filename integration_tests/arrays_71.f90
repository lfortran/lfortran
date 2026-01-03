program arrays_71
integer :: x1(5) = [2,1,3,3,2]
integer :: x2(3,3) = reshape([1,10,11,22,1,2,1,1,2], [3,3])
integer :: y(3) = [0, 0, 0]
integer :: y2(5) = [0, 0, 0, 0, 0]
integer :: z = 2
y = [z, x2(1,1), x2(1,2)]
if(y(1) /= 2 .or. y(2) /= 1 .or. y(3) /= 22) error stop
y = [z, x1([1,2])]
if(y(1) /= 2 .or. y(2) /= 2 .or. y(3) /= 1) error stop
y = [z, x2([1,2],1)]
if(y(1) /= 2 .or. y(2) /= 1 .or. y(3) /= 10) error stop
y2 = [z, x2([1,2],[1,2])]
if(y2(1) /= 2 .or. y2(2) /= 1 .or. y2(3) /= 10 .or. y2(4) /= 22 .or. y2(5) /= 1) error stop
y2 = [z, x2(2:3,2:3)]
if(y2(1) /= 2 .or. y2(2) /= 1 .or. y2(3) /= 2 .or. y2(4) /= 1 .or. y2(5) /= 2) error stop
y2 = [z, x2(1:2,[1,2])]
if(y2(1) /= 2 .or. y2(2) /= 1 .or. y2(3) /= 10 .or. y2(4) /= 22 .or. y2(5) /= 1) error stop

end program