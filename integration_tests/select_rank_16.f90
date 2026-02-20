module select_rank_16_mod
  implicit none

  type :: my_t
    integer :: val = 0
  end type

contains

  ! Test assumed-rank with derived type and function call in rank block
  function process_rank1(items) result(res)
    type(my_t), intent(in) :: items(..)
    type(my_t) :: res
    select rank(items)
      rank(1)
        res = helper1(items)
    end select
  contains
    function helper1(arr) result(r)
      type(my_t), intent(in) :: arr(:)
      type(my_t) :: r
      r%val = size(arr)
    end function
  end function

  ! Test assumed-rank with integer and subroutine call in rank block
  subroutine sum_rank1(items, total)
    integer, intent(in) :: items(..)
    integer, intent(out) :: total
    select rank(items)
      rank(1)
        call do_sum(items, total)
    end select
  contains
    subroutine do_sum(arr, s)
      integer, intent(in) :: arr(:)
      integer, intent(out) :: s
      integer :: i
      s = 0
      do i = 1, size(arr)
        s = s + arr(i)
      end do
    end subroutine
  end subroutine

  ! Test assumed-rank logical type
  function count_true(flags) result(n)
    logical, intent(in) :: flags(..)
    integer :: n
    select rank(flags)
      rank(1)
        n = count(flags)
    end select
  end function

end module

program select_rank_16
  use select_rank_16_mod
  implicit none
  type(my_t) :: arr(3), r
  integer :: iarr(4), total
  logical :: larr(5)

  arr(1)%val = 10
  arr(2)%val = 20
  arr(3)%val = 30
  r = process_rank1(arr)
  if (r%val /= 3) error stop

  iarr = [1, 2, 3, 4]
  call sum_rank1(iarr, total)
  if (total /= 10) error stop

  larr = [.true., .false., .true., .true., .false.]
  if (count_true(larr) /= 3) error stop

  print *, "PASS"
end program
