program get_environment_variable_02
  ! Trailing blanks in NAME are insignificant unless TRIM_NAME is .false.
  implicit none
  character(16) :: name
  character(:), allocatable :: ref, val
  integer :: length, ref_length, status
  logical :: trim_flag

  call get_environment_variable("PATH", length=ref_length, status=status)
  if (status /= 0) error stop
  if (ref_length <= 0) error stop
  allocate(character(ref_length) :: ref, val)
  call get_environment_variable("PATH", ref)

  name = "PATH"

  call get_environment_variable(name, status=status)
  if (status /= 0) error stop
  call get_environment_variable(name, length=length)
  if (length /= ref_length) error stop
  call get_environment_variable(name, val, length=length, status=status)
  if (status /= 0) error stop
  if (length /= ref_length) error stop
  if (val /= ref) error stop

  call get_environment_variable(name, val, length=length, status=status, &
      trim_name=.true.)
  if (status /= 0) error stop
  if (length /= ref_length) error stop
  if (val /= ref) error stop

  call get_environment_variable(name, val, length=length, status=status, &
      trim_name=.false.)
  if (status == 0) error stop
  if (length /= 0) error stop
  if (len_trim(val) /= 0) error stop
  call get_environment_variable(name, length=length, trim_name=.false.)
  if (length /= 0) error stop
  call get_environment_variable(name, status=status, trim_name=.false.)
  if (status == 0) error stop

  trim_flag = .true.
  call get_environment_variable(name, val, length=length, status=status, &
      trim_name=trim_flag)
  if (status /= 0) error stop
  if (length /= ref_length) error stop
  if (val /= ref) error stop

  trim_flag = .false.
  call get_environment_variable(name, val, length=length, status=status, &
      trim_name=trim_flag)
  if (status == 0) error stop
  if (length /= 0) error stop

  print *, "ok"
end program get_environment_variable_02
