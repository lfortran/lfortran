module list_test_06_mod
   implicit none
   real::eps = 1e-6

contains

   subroutine check_mat_and_vec(mat, vec)
      type(_lfortran_list(_lfortran_list(real))), intent(in) :: mat
      type(_lfortran_list(real)), intent(in) :: vec
      integer::rows, cols, i, j

      rows = _lfortran_len(mat)
      cols = _lfortran_len(vec)

      do i = 0, rows-1
         do j = 0, cols-1
            if ( abs(_lfortran_get_item(_lfortran_get_item(mat, i), j) - real(i+j)) > eps ) error stop
         end do
      end do

      do i = 0, cols-1
         if ( abs(_lfortran_get_item(vec, i) - 2*real(i)) > eps ) error stop
      end do
   end subroutine

   subroutine test_list_of_lists()
      type(_lfortran_list(_lfortran_list(_lfortran_list(_lfortran_list(real))))) :: arrays
      type(_lfortran_list(_lfortran_list(_lfortran_list(real)))) :: array
      type(_lfortran_list(_lfortran_list(real))) :: mat
      type(_lfortran_list(real)) :: vec
      integer:: rows, cols, i, j, k, l
      real:: tmp, tmp1

      rows = 10
      cols = 5

      do i = 0, rows-1
         do j = 0, cols-1
            call _lfortran_list_append(vec, real(i+j))
         end do
         call _lfortran_list_append(mat, vec)
         call _lfortran_clear(vec)
      end do


      do i = 0, cols-1
         call _lfortran_list_append(vec, 2.0*real(i))
      end do

      call check_mat_and_vec(mat, vec)


      do k = 0, rows-1
         call _lfortran_list_append(array, mat)
         do i = 0, rows-1
            do j = 0, cols-1
               tmp = _lfortran_get_item(_lfortran_get_item(mat, i), j)
               call _lfortran_set_item(_lfortran_get_item(mat, i), j, tmp + 1.0)
            end do
         end do
      end do


      do k = 0, rows-1
         do i = 0, rows-1
            do j = 0, cols-1
               tmp = _lfortran_get_item(_lfortran_get_item(mat, i), j)
               tmp1 = _lfortran_get_item(_lfortran_get_item(_lfortran_get_item(array, k), i), j)

               if ( abs(tmp-tmp1-real(rows-k)) > eps ) error stop
            end do
         end do
      end do


      do l=0, 2*rows-1
         call _lfortran_list_append(arrays, array)
         do i = 0, rows-1
            do j = 0, rows-1
               do k = 0, cols-1
                  tmp = _lfortran_get_item(_lfortran_get_item(_lfortran_get_item(array, i), j), k)
                  call _lfortran_set_item(_lfortran_get_item(_lfortran_get_item(array, i), j), k, tmp+1.0)
               end do
            end do
         end do
      end do


      do l=0, 2*rows-1
         do i = 0, rows-1
            do j = 0, rows-1
               do k = 0, cols-1
                  tmp = _lfortran_get_item(_lfortran_get_item(_lfortran_get_item(array, i), j), k)
                  tmp1 = _lfortran_get_item(_lfortran_get_item(_lfortran_get_item(_lfortran_get_item(arrays, l), i), j), k)

                  if ( abs(tmp-tmp1-real(2*rows-l)) > eps ) error stop
               end do
            end do
         end do
      end do
   end subroutine
end module

program test_list_06_
   use list_test_06_mod
   implicit none

   call test_list_of_lists
end program
