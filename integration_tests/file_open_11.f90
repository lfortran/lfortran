program file_open_11
   implicit none
   integer :: u, ios
   logical :: ex
   open (newunit=u, file="no_such_dir_xyz/out.bin", form="unformatted", &
         access="stream", status="replace", action="write", iostat=ios)
   if (ios == 0) error stop
end program file_open_11