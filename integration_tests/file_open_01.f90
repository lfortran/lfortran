! Test using action parameter with OPEN statement
program file_open_01
    integer :: u
    character(:), allocatable :: action_
    open(u, action=action_)
  end program