module tomlf_utils_sort
   implicit none

   type :: toml_key
      character(len=:), allocatable :: key
   end type toml_key

   interface sort
      module procedure :: sort_keys
   end interface


   abstract interface
      pure function compare_less(lhs, rhs) result(less)
         import :: toml_key
         type(toml_key), intent (in) :: lhs
         type(toml_key), intent (in) :: rhs
         logical :: less
      end function compare_less
   end interface

contains


   pure subroutine sort_keys(list, idx, compare)
      type(toml_key), intent(inout) :: list(:)
      integer, intent(out), optional :: idx(:)
      procedure(compare_less), optional :: compare

      integer  :: low, high, i
      type(toml_key), allocatable  :: sorted(:)
      integer, allocatable :: indexarray(:)

      low = 1
      high = size(list)

      sorted = list

      allocate(indexarray(high), source=[(i, i=low, high)])

      if (present(compare)) then
         call quicksort(sorted, indexarray, low, high, compare)
      else
         call quicksort(sorted, indexarray, low, high, compare_keys_less)
      end if

      do i = low, high
         list(i) = sorted(indexarray(i))
      end do

      if (present(idx)) then
         idx = indexarray
      end if

   end subroutine sort_keys

   pure recursive subroutine quicksort(list, idx, low, high, less)
      type(toml_key), intent(inout) :: list(:)
      integer, intent(inout) :: idx(:)
      integer, intent(in) :: low, high
      procedure(compare_less) :: less

      integer :: i, last
      integer :: pivot

      if (low < high) then

         call swap(idx(low), idx((low + high)/2))
         last = low
         do i = low + 1, high
            if (less(list(idx(i)), list(idx(low)))) then
               last = last + 1
               call swap(idx(last), idx(i))
            end if
         end do
         call swap(idx(low), idx(last))
         pivot = last

         call quicksort(list, idx, low, pivot - 1, less)
         call quicksort(list, idx, pivot + 1, high, less)
      end if

   end subroutine quicksort

   pure subroutine swap(lhs, rhs)
      integer, intent(inout) :: lhs
      integer, intent(inout) :: rhs

      integer :: tmp

      tmp = lhs
      lhs = rhs
      rhs = tmp

   end subroutine swap

   pure function compare_keys_less(lhs, rhs) result(less)
      type(toml_key), intent (in) :: lhs
      type(toml_key), intent (in) :: rhs
      logical :: less

      less = lhs%key < rhs%key

   end function compare_keys_less

end module tomlf_utils_sort

program functions_16

print *, "running functions_16 program"

end program
