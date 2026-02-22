module submodule_22_mod
  implicit none

  interface
    pure module function separated_values(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function
  end interface

end module submodule_22_mod

submodule(submodule_22_mod) submodule_22_sub
  implicit none
contains
  pure module function separated_values(separator, mold) result(format_string)
    character(len=*), intent(in) :: separator
    class(*), intent(in) :: mold(..)
    character(len=:), allocatable :: format_string
    format_string = "(*(G25.20,:,'" // separator // "'))"
  end function
end submodule submodule_22_sub

program submodule_22
  use submodule_22_mod
  implicit none
  character(len=100) :: output
  real, parameter :: vals(*) = [1.0, 2.0, 3.0]

  write(output, fmt=separated_values(separator=",", mold=[real::])) vals
  if (len_trim(output) == 0) error stop
  print *, trim(output)
end program submodule_22
