module assumed_rank_16_m
   implicit none
contains
   subroutine elementcopy_real32(a1,a2)
      real,intent(in)       :: a1(..)
      real                  :: a2(..)
      select rank(a1)
         rank(1)
         select rank(a2)
            rank(0); call step2(a1)
         end select
      end select
      contains
      subroutine step2(aa3)
         real,intent(in)    :: aa3(:)
         select rank(a2)
            rank(1); call ecopy(aa3,a2)
         end select
      end subroutine step2
      subroutine ecopy(aaa1,aaa2)
         real,intent(in)    :: aaa1(:)
         real               :: aaa2(:)
      end subroutine ecopy
end subroutine elementcopy_real32
end module assumed_rank_16_m
subroutine empty()
end subroutine empty

program assumed_rank_16
   use m_bug
   implicit none

   real :: a1(10)
   real :: a2

   a1 = 1.0
   a2 = 2.0

   call elementcopy_real32(a1, a2)

   error stop "Reached end of main"

end program assumed_rank_16