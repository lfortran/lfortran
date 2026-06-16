module intrinsics_471_mod
   implicit none
contains
   subroutine sub_a(s, expected)
      character(len=*), intent(in) :: s, expected
      character(len=:), allocatable :: a, b, c
      a = trim(s)
      b = trim(s)
      c = trim(s)
      if (a /= expected) error stop 11
      if (b /= expected) error stop 12
      if (c /= expected) error stop 13
      if (len_trim(s) /= len(expected)) error stop 14
      if (len_trim(s) /= len(expected)) error stop 15
   end subroutine

   subroutine sub_b(s, expected)
      character(len=*), intent(in) :: s, expected
      character(len=:), allocatable :: a, b
      a = trim(s)
      b = trim(s)
      if (a /= expected) error stop 21
      if (b /= expected) error stop 22
      if (len_trim(s) /= len(expected)) error stop 23
   end subroutine

   subroutine sub_c(s, expected)
      character(len=*), intent(in) :: s, expected
      character(len=:), allocatable :: a
      a = trim(s)
      if (a /= expected) error stop 31
      if (len_trim(s) /= len(expected)) error stop 32
   end subroutine
end module

program intrinsics_471
   use intrinsics_471_mod
   implicit none
   character(len=20) :: s
   character(len=:), allocatable :: t1, t2, t3
   integer :: i

   s = "hello world"
   call sub_a(s, "hello world")
   call sub_a(s, "hello world")
   call sub_b(s, "hello world")
   call sub_c(s, "hello world")

   t1 = trim(s)
   t2 = trim(s)
   t3 = trim(s) // " | " // trim(s)
   if (t1 /= "hello world") error stop 41
   if (t2 /= "hello world") error stop 42
   if (t3 /= "hello world | hello world") error stop 43
   if (len_trim(s) /= 11) error stop 44
   if (len_trim(s) /= 11) error stop 45

   do i = 1, 5
      t1 = trim(s)
      if (t1 /= "hello world") error stop 50 + i
      if (len_trim(s) /= 11) error stop 60 + i
   end do
end program intrinsics_471
