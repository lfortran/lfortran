module testdrive_intrinsics_380
  implicit none

contains

  !> Obtain the value of an environment variable
  subroutine get_variable(var, val)

    !> Name of variable
    character(len=*), intent(in) :: var

    !> Value of variable
    character(len=:), allocatable, intent(out) :: val

    integer :: length, stat

    call get_environment_variable(var, length=length, status=stat)

    allocate(character(len=length) :: val, stat=stat)

    if (length > 0) then
      call get_environment_variable(var, val, status=stat)
    end if

  end subroutine get_variable

end module testdrive_intrinsics_380

program intrinsics_380
  use testdrive_intrinsics_380
  character(len=:), allocatable :: val

  call get_variable("LFORTRAN_TEST_ENV_VAR", val)
  write(*,*) trim(val)
  if (trim(val) /= "STATUS OK!") error stop
end program
