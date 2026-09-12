module select_rank_41_mod

   implicit none

contains

   ! Arithmetic and relational expressions involving the selector of an
   ! enclosing `select rank` block, including nested `select rank` blocks.
   function add_reduce(a, b) result(res)
      real(8), intent(in) :: a(..)
      real(8), intent(in) :: b(..)
      real(8) :: res
      res = -1.d0
      select rank(a)
         rank(1)
            select rank(b)
               rank(1)
                  res = sum(a + b)
            end select
         rank(2)
            select rank(b)
               rank(2)
                  res = sum(a + b)
            end select
      end select
   end function add_reduce

   function neg_sum(a) result(res)
      real(8), intent(in) :: a(..)
      real(8) :: res
      res = -1.d0
      select rank(a)
         rank(1)
            res = sum(-a)
         rank(2)
            res = sum(-a)
      end select
   end function neg_sum

   ! A `rank(0)` selector is a scalar, so it takes part in scalar arithmetic.
   function twice(a) result(res)
      real(8), intent(in) :: a(..)
      real(8) :: res
      res = -1.d0
      select rank(a)
         rank(0)
            res = a + a
      end select
   end function twice

   function count_above(a, x) result(res)
      real(8), intent(in) :: a(..)
      real(8), intent(in) :: x
      integer :: res
      res = -1
      select rank(a)
         rank(1)
            res = count(a + a > x)
      end select
   end function count_above

end module select_rank_41_mod

program select_rank_41

   use select_rank_41_mod
   implicit none

   real(8) :: a(4), b(4)
   real(8) :: c(2,2), d(2,2)
   real(8) :: e(4)

   a = 1.d0; b = 2.d0
   c = 1.d0; d = 2.d0
   e = [1.d0, 2.d0, 3.d0, 4.d0]

   if (abs(add_reduce(a, b) - 12.d0) > 1.d-12) error stop
   if (abs(add_reduce(c, d) - 12.d0) > 1.d-12) error stop

   if (abs(neg_sum(a) + 4.d0) > 1.d-12) error stop
   if (abs(neg_sum(c) + 4.d0) > 1.d-12) error stop

   if (abs(twice(3.d0) - 6.d0) > 1.d-12) error stop

   if (count_above(e, 4.d0) /= 2) error stop

end program select_rank_41
