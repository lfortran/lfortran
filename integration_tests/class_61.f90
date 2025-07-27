program class_61
  implicit none

    type, abstract :: fpm_cmd_settings
    end type
  type, extends(fpm_cmd_settings) :: fpm_run_settings
    character(len=3), allocatable :: name(:)
  end type fpm_run_settings

  class(fpm_cmd_settings), allocatable :: settings
  character(len=3), allocatable :: arr(:)

  allocate(arr(3))
    arr = ["abc", "def", "ghi"]
  allocate(fpm_run_settings :: settings)
  settings = fpm_run_settings(name=arr)
  select type(settings)
    type is (fpm_run_settings)
      if (settings%name(1) /= "abc") error stop
      if (settings%name(2) /= "def") error stop
      if (settings%name(3) /= "ghi") error stop
    class default
      error stop
  end select
end program class_61