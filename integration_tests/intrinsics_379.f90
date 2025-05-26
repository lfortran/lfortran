module testdrive_intrinsics_379

  contains

  subroutine get_argument(idx, arg)

    integer, intent(in) :: idx

    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg

    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)

    call get_command_argument(idx, arg, status=stat)

  end subroutine get_argument

end module testdrive_intrinsics_379

program intrinsics_379
  use testdrive_intrinsics_379

  implicit none

  character(len=:), allocatable :: arg1, arg2

  ! Get the first command line argument
  call get_argument(1, arg1)

  ! Get the second command line argument
  call get_argument(2, arg2)

end program
