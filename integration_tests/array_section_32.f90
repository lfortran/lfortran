program array_section_32
integer,parameter :: arr(2,2)=reshape([10,20,30,40],[2,2])
integer,parameter :: sub(*)=pack(arr(:,1),.true.)

if (size(sub) /= 2) error stop
if (sub(1) /= 10) error stop
if (sub(2) /= 20) error stop

end program array_section_32
