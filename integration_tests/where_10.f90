program where_10
   implicit none
   real, parameter :: zero = 0.0

   integer :: i

   real :: first_array(1, 4)
   integer :: second_array(2, 4)
   logical :: third_array(2, 4)

   real :: first_output(1, 4)
   integer :: second_output(2, 4)
   logical :: third_output(2, 4)

   first_array = reshape([0.0, 1.0, 0.0, 1.0], [1, 4])
   second_array = reshape([1, 2, 0, 4, 5, 0, 7, 0], [2, 4])
   third_array = reshape([.false., .true., .true., .true., .false., .true., .false., .true.], [2, 4])


   where (first_array(1, :) == zero)
      first_array(1, :) = 2.0
   end where

   print *, first_array
   first_output = reshape([2.0, 1.0, 2.0, 1.0], [1, 4])
   if (all(first_array /= first_output)) error stop

   where (second_array(:, 4) /= 0)
      second_array(:, 4) = 22
   end where

   print *, second_array
   second_output = reshape([1, 2, 0, 4, 5, 0, 22, 0], [2, 4])
   if (all(second_array /= second_output)) error stop

   i = 1
   where (first_array(i, :) > 1.0)
      first_array(i, :) = 22.0
   end where

   print *, first_array
   first_output = reshape([22.0, 1.0, 22.0, 1.0], [1, 4])
   if (all(first_array /= first_output)) error stop

   where (third_array(2, :))
      second_array(2, :) = 1
   end where

   print *, second_array
   second_output = reshape([1, 1, 0, 1, 5, 1, 22, 1], [2, 4])
   if (all(second_array /= second_output)) error stop

   where (third_array(2, :) .neqv. .false.)
      third_array(2, :) = .false.
   end where

   print *, third_array
   third_output = reshape([.false., .false., .true., .false., .false., .false., .false., .false.], [2, 4])
   if (all(third_array .neqv. third_output)) error stop


   ! Assignment like:
   !     first_array(1, :) = first_array(1, :) + 1
   ! is currently not supported inside the `WHERE` clause.
   !
   ! Uncomment after supporting the above:
   !
   where (first_array(1, :) > 1.0)
      first_array(1, :) = first_array(1, :) + 1
   end where

   print *, first_array
   first_output = reshape([3.0, 1.0, 3.0, 1.0], [1, 4])
   if (all(first_array /= first_output)) error stop
   !
   ! =========================================================
   !
   ! Array section expressions like second_array(:, :)
   ! is currently not supported inside `WHERE` clause.
   !
   ! Uncomment after supporting the above:
   !
   where (second_array(:, :) /= 0)
      second_array(:, :) = 10
   end where

   print *, second_array
   second_output = reshape([10, 10, 0, 10, 10, 0, 10, 0], [2, 4])
   if (all(second_array /= second_output)) error stop


end program where_10
