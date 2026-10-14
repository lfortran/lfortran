module data_25_mod
   implicit none
contains
   integer function data_25_next() result(r)
      integer :: q(2, 2), i, j
      data ((q(i, j), i = 1, 2), j = 1, 2) / 1, 2, 3, 4 /
      r = 10 * q(1, 2) + q(2, 1)
      q(1, 2) = q(1, 2) + 1
   end function data_25_next
end module data_25_mod

subroutine data_25_tab(n, total, text)
   implicit none
   integer, intent(in) :: n
   integer, intent(out) :: total
   character(len=4), intent(out) :: text
   integer :: t(4), z(3), w(4), k
   real :: r(3)
   character(len=4) :: c(2), d
   data t / 10, 20, 30, 40 /
   data z / 3*0 /
   data w(2:3) / 7, 8 /, w(4) / 9 /
   data (r(k), k = 1, 3) / 1.0, 2.0, 3.0 /
   data c / 'ab', 'cdef' /, d(2:3) / 'xy' /
   total = t(n) + z(n) + w(n + 1) + w(4) + int(r(n))
   text = c(n)(1:2) // d(2:3)
   t(n) = t(n) + 100
   z(n) = z(n) + 1
   w(n + 1) = w(n + 1) + 1000
   r(n) = r(n) + 10
   c(n) = 'zz'
   d = 'abcd'
end subroutine data_25_tab

program data_25
   use data_25_mod
   implicit none
   integer :: total
   character(len=4) :: text
   call data_25_tab(1, total, text)
   if (total /= 27 .or. text /= 'abxy') error stop 1
   call data_25_tab(1, total, text)
   if (total /= 1138 .or. text /= 'zzbc') error stop 2
   call data_25_tab(2, total, text)
   if (total /= 39 .or. text /= 'cdbc') error stop 3
   if (data_25_next() /= 32) error stop 4
   if (data_25_next() /= 42) error stop 5
   print *, "ok"
end program data_25
