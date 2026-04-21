module read_78_mod
   implicit none
contains
   subroutine get_sarr(val, arr)
      character(len=*), intent(in) :: val
      character(len=*), allocatable, intent(out) :: arr(:)

      if (.not. allocated(arr)) allocate(arr(2))
      read(val, *) arr
   end subroutine get_sarr
end module read_78_mod

program read_78
   use read_78_mod, only: get_sarr
   implicit none
   character(len=16), allocatable :: arr(:)

   call get_sarr('alpha, beta', arr)

   if (trim(arr(1)) /= 'alpha' .or. trim(arr(2)) /= 'beta') error stop
end program read_78
