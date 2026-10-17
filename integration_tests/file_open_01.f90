! Test using action parameter with OPEN statement
program file_open_01
    integer :: u
    character(:), allocatable :: action_
    u = 10
    action_ = "readwrite"
    open(u, file="file_open_01.txt", action=action_)
    close(u, status="delete")
  end program