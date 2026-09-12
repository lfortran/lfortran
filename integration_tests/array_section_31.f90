program array_section_31
integer,parameter :: arr(2,2)=reshape([10,20,30,40],[2,2])
integer,parameter :: sub(*)=arr(:,1)

if (size(sub) /= 2) error stop
if (sub(1) /= 10) error stop
if (sub(2) /= 20) error stop

end program array_section_31
