module select_rank_33_mod
   implicit none
   type :: graph_type
      integer :: x = 0
   end type graph_type
contains
   subroutine handle(input, expected_rank, ok)
      class(*), dimension(..), intent(in) :: input
      integer, intent(in) :: expected_rank
      logical, intent(inout) :: ok
      logical :: matched
      matched = .false.
      select rank(input)
      rank(1)
         select type(input)
         type is(graph_type)
            if (size(input) == 3) then
               if (input(1)%x == 1 .and. input(2)%x == 2 .and. input(3)%x == 3) then
                  matched = .true.
               end if
            end if
         end select
         if (expected_rank /= 1) ok = .false.
      rank(2)
         select type(input)
         type is(graph_type)
            if (size(input, 1) == 1 .and. size(input, 2) == 2) then
               if (input(1, 1)%x == 10 .and. input(1, 2)%x == 20) then
                  matched = .true.
               end if
            end if
         end select
         if (expected_rank /= 2) ok = .false.
      end select
      if (.not. matched) ok = .false.
   end subroutine handle
end module select_rank_33_mod

program select_rank_33
   use select_rank_33_mod
   implicit none
   type(graph_type), dimension(3) :: rank1
   type(graph_type), dimension(1, 2) :: rank2
   logical :: ok

   rank1(1)%x = 1
   rank1(2)%x = 2
   rank1(3)%x = 3
   rank2(1, 1)%x = 10
   rank2(1, 2)%x = 20

   ok = .true.
   call handle(rank1, 1, ok)
   call handle(rank2, 2, ok)
   if (.not. ok) error stop "select type inside select rank failed"
   print *, "PASS"
end program select_rank_33
