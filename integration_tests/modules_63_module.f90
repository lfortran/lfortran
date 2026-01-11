module mre_read_string_fmt_mod
contains
   subroutine temp()
      character(len=:), allocatable :: str
      str = "'control char "//achar(0)//"','normal literal'"
      if (len(str) /= 33) error stop
   end subroutine temp
end module mre_read_string_fmt_mod

