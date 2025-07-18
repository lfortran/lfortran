! Test using action parameter with OPEN statement
program file_open_01
    integer :: u
    character(:), allocatable :: filename
    character(:), allocatable :: action_
    open(u, file=filename, action=action_)
  end program