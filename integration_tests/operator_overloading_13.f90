module operator_overloading_13_module
  implicit none
  private
  public :: operator(.separatedBy.)

  interface operator(.separatedBy.)
    module function strings_with_separator(sep, strings) result(csv)
      implicit none
      character(len=*), intent(in) :: sep
      character(len=*), dimension(:), intent(in) :: strings
      character(len=:), allocatable :: csv
    end function
  end interface

contains

  module function strings_with_separator(sep, strings) result(csv)
    implicit none
    character(len=*), intent(in) :: sep
    character(len=*), dimension(:), intent(in) :: strings
    character(len=:), allocatable :: csv

    integer :: i
    character(len=:), allocatable :: temp

    csv = ""
    do i = 1, size(strings)
      if (i > 1) csv = csv // sep
      csv = csv // trim(strings(i))
    end do
  end function

end module operator_overloading_13_module

program operator_overloading_13
  use operator_overloading_13_module, only: operator(.separatedBy.)
  implicit none

  character(len=:), allocatable :: csv
  character(len=*), dimension(3), parameter :: strings = ["abc", "def", "ghi"]

  csv = "," .separatedBy. strings
  print *, "CSV Result: ", trim(csv)
  if (trim(csv) /= "abc,def,ghi") error stop

end program
