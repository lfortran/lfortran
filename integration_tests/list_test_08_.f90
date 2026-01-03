module list_test_08_mod
   implicit none
   real :: eps = 1e-6

contains

   function l1norm(v) result(res)
      type(_lfortran_list(integer)), intent(in) :: v
      real :: res
      integer :: i

      res = 0.0
      do i = 0, _lfortran_len(v) - 1
         res = res + real(_lfortran_get_item(v, i))
      end do
   end function

   function sort(l) result(res)
      type(_lfortran_list(integer)), intent(inout) :: l
      type(_lfortran_list(integer)) :: res
      integer :: i, j
      integer :: a, b

      res = l

      do i = 0, _lfortran_len(res) - 1
         do j = i + 1, _lfortran_len(res) - 1
            a = _lfortran_get_item(res, i)
            b = _lfortran_get_item(res, j)
            if (a > b) then
               call _lfortran_set_item(res, i, b)
               call _lfortran_set_item(res, j, a)
            end if
         end do
      end do
   end function

   subroutine sort_list()
      type(_lfortran_list(integer)) :: x
      integer :: size, i

      size = 50
      do i = 0, size - 1
         call _lfortran_list_append(x, size - i)
      end do

      x = sort(x)

      do i = 0, size - 2
         if (_lfortran_get_item(x, i) > _lfortran_get_item(x, i + 1)) error stop
      end do

      if (_lfortran_len(x) /= size) error stop
   end subroutine

   subroutine sort_by_l1_norm()
      type(_lfortran_list(_lfortran_list(integer))) :: mat
      type(_lfortran_list(integer)) :: vec, temp
      integer :: i, j, k, rows, cols
      real :: norm1, norm2

      rows = 10
      cols = 7
      k = rows * cols

      do i = 0, rows - 1
         do j = 0, cols - 1
            call _lfortran_list_append(vec, k)
            k = k - 1
         end do
         call _lfortran_list_append(mat, vec)
         call _lfortran_clear(vec)
      end do

      do i = 0, rows - 1
         do j = i + 1, rows - 1
            norm1 = l1norm(_lfortran_get_item(mat, i))
            norm2 = l1norm(_lfortran_get_item(mat, j))
            if (norm1 > norm2) then
               temp = sort(_lfortran_get_item(mat, j))
               call _lfortran_set_item(mat, j, sort(_lfortran_get_item(mat, i)))
               call _lfortran_set_item(mat, i, temp)
            end if
         end do
      end do

      k = 1
      do i = 0, rows - 1
         do j = 0, cols - 1
            if (_lfortran_get_item(_lfortran_get_item(mat, i), j) /= k) error stop
            k = k + 1
         end do
      end do
   end subroutine

   subroutine tests()
      call sort_list()
      call sort_by_l1_norm()
   end subroutine

end module

program test_sort_lists
   use list_test_08_mod
   implicit none

   call tests
end program
