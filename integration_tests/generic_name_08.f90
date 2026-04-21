module generic_name_08_path_mod
  implicit none
  type :: toml_key
    character(:), allocatable :: s
  end type
  type :: toml_path
    type(toml_key), allocatable :: path(:)
  end type
  interface toml_path
    module procedure new_path2
  end interface
contains
  function new_path2(a, b) result(p)
    character(*), intent(in) :: a, b
    type(toml_path) :: p
    allocate(p%path(2))
    p%path(1)%s = a
    p%path(2)%s = b
  end function
end module

module generic_name_08_build_mod
  use generic_name_08_path_mod, only : toml_path, toml_key
  implicit none
  public :: toml_path, toml_key
end module

module generic_name_08_top_mod
  use generic_name_08_build_mod, only : toml_path, toml_key
  implicit none
  public :: toml_path, toml_key
end module

program generic_name_08
  use generic_name_08_build_mod
  use generic_name_08_top_mod
  implicit none
  type(toml_path) :: p
  p = toml_path([toml_key("x"), toml_key("y")])
  if (size(p%path) /= 2) error stop
  if (p%path(1)%s /= "x") error stop
  if (p%path(2)%s /= "y") error stop
  print *, "PASS"
end program
