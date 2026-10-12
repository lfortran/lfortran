module intrinsics_len_01_mod
   implicit none
   type :: holder
      character(len=4), allocatable :: names(:)
   end type holder
   type :: item
      character(len=6) :: s
   end type item
contains
   integer function len_lb0(a) result(r)
      character(len=*), intent(in) :: a(0:)
      character(len=len(a)) :: tmp
      if (size(a) /= 0) error stop
      if (len(tmp) /= len(a)) error stop
      r = len(a)
   end function len_lb0

   integer function len_1d(a) result(r)
      character(len=*), intent(in) :: a(:)
      r = len(a)
   end function len_1d

   integer function len_2d(a) result(r)
      character(len=*), intent(in) :: a(:,:)
      r = len(a)
   end function len_2d

   integer function len_explicit(n, a) result(r)
      integer, intent(in) :: n
      character(len=*), intent(in) :: a(n)
      r = len(a)
   end function len_explicit

   function pad(a) result(r)
      character(len=*), intent(in) :: a(:)
      character(len=len(a) + 1) :: r
      r = repeat("x", len(r))
   end function pad

   subroutine len_array_ops(a, n)
      character(len=*), intent(in) :: a(:)
      integer, intent(in) :: n
      integer :: u(3), v(3)
      u = [1, 2, 3]
      v = len(a) * u
      if (any(v /= n * u)) error stop
      v = u + len(a)
      if (any(v /= n + u)) error stop
      v = max(len(a), u)
      if (any(v /= max(n, u))) error stop
      v = 0
      where (u < len(a)) v = 1
      if (any(v /= merge(1, 0, u < n))) error stop
      v = 0
      v = v + len(a) * [1, 2, 3]
      if (any(v /= n * u)) error stop
   end subroutine len_array_ops

   subroutine take(w, expect)
      integer, intent(in) :: w(:), expect(:)
      if (size(w) /= size(expect)) error stop
      if (any(w /= expect)) error stop
   end subroutine take

   subroutine take_real(w, expect)
      real, intent(in) :: w(:), expect(:)
      if (size(w) /= size(expect)) error stop
      if (any(w /= expect)) error stop
   end subroutine take_real

   subroutine len_runtime_shape_ops(a, u, n)
      character(len=*), intent(in) :: a(:)
      integer, intent(in) :: u(:)
      integer, intent(in) :: n
      integer, allocatable :: ua(:), w(:)
      character(len=40) :: buf, expect
      call take(len(a) * u, n * u)
      call take(-len(a) + u, u - n)
      call take_real(real(len(a)) * real(u), real(n * u))
      if (sum(len(a) * u) /= n * sum(u)) error stop
      if (maxval(len(a) + u) /= n + maxval(u)) error stop
      if (dot_product(len(a) * u, u) /= n * dot_product(u, u)) error stop
      if (size(pack(len(a) * u, u > 1)) /= count(u > 1)) error stop
      if (any(pack(len(a) * u, u > 1) /= pack(n * u, u > 1))) error stop
      write(buf, "(10i4)") len(a) + u
      write(expect, "(10i4)") n + u
      if (buf /= expect) error stop
      allocate(w(size(u)))
      w = 0
      where (u > 100)
         w = 1
      elsewhere
         w = len(a) * u
      end where
      if (size(w) /= size(u)) error stop
      if (any(w /= n * u)) error stop
      allocate(ua(size(u)))
      ua = u
      call take(len(a) * ua, n * u)
      if (sum(len(a) * ua) /= n * sum(u)) error stop
      write(buf, "(10i4)") len(a) * ua
      write(expect, "(10i4)") n * u
      if (buf /= expect) error stop
   end subroutine len_runtime_shape_ops
end module intrinsics_len_01_mod

program intrinsics_len_01
   use intrinsics_len_01_mod
   implicit none
   character(len=5) :: empty(0)
   character(len=3) :: full(4)
   character(len=6) :: grid(0, 3)
   character(len=:), allocatable :: dal(:), d5(:)
   character(len=2) :: one(1)
   integer :: w(2)
   type(holder) :: h
   type(item) :: items(3)

   full = "abc"

   if (len_lb0(empty) /= 5) error stop
   if (len_1d(empty) /= 5) error stop
   if (len_1d(full) /= 3) error stop
   if (len_1d(full(3:2)) /= 3) error stop
   if (len_2d(grid) /= 6) error stop
   if (len_explicit(0, empty) /= 5) error stop
   if (len_explicit(4, full) /= 3) error stop
   if (len(pad(empty)) /= 6) error stop
   if (pad(full) /= "xxxx") error stop

   allocate(character(len=7) :: dal(0))
   if (len(dal) /= 7) error stop

   allocate(h%names(0))
   if (len(h%names) /= 4) error stop

   if (len(items%s) /= 6) error stop
   if (len(items(1:2)%s) /= 6) error stop
   if (len(items(3:2)%s) /= 6) error stop

   call len_array_ops(full, 3)
   call len_array_ops(full(1:2), 3)
   call len_array_ops(one, 2)
   call len_array_ops(empty, 5)

   call len_runtime_shape_ops(full, [1, 2, 3], 3)
   call len_runtime_shape_ops(full(1:2), [1, 2, 3], 3)
   call len_runtime_shape_ops(one, [4, 5, 6], 2)
   call len_runtime_shape_ops(empty, [1, 2, 3], 5)

   allocate(character(len=4) :: d5(5))
   w = len(d5) + [10, 20]
   if (any(w /= [14, 24])) error stop

   print *, len_lb0(empty), len_1d(full), len_2d(grid), len(dal)
end program intrinsics_len_01
