program read_44
      use iso_fortran_env, only: int64
      implicit none

      type :: string_t
          character(len=:), allocatable :: str
      end type

      type(string_t) :: val
      integer(int64) :: i1, istat

      val%str = "111"
      read(val%str, *, iostat=istat) i1  
      print*, i1, istat
      if (i1 /= 111) error stop
      if (istat /= 0) error stop

  end program read_44