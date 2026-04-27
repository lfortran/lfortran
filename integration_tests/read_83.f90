program read_83
implicit none
character(len=256) :: buffer
real, allocatable :: data_list(:)
integer :: j, stat, c, k
integer, dimension(2) :: padding, stride

! Test 1: implied-do-loop reading from internal file into allocatable array
allocate(data_list(10), source=0.0)
buffer = "1.0 2.0 3.0 4.0 5.0"
c = 1
k = 5
read(buffer, *, iostat=stat) (data_list(j), j=c, c+k-1)
if (stat /= 0) error stop
if (abs(data_list(1) - 1.0) > 1e-5) error stop
if (abs(data_list(3) - 3.0) > 1e-5) error stop
if (abs(data_list(5) - 5.0) > 1e-5) error stop
deallocate(data_list)

! Test 2: reading fixed-size integer array from internal file
buffer = "3 5"
read(buffer, *) padding
if (padding(1) /= 3) error stop
if (padding(2) /= 5) error stop

buffer = "2 4"
read(buffer, *) stride
if (stride(1) /= 2) error stop
if (stride(2) /= 4) error stop

end program read_83