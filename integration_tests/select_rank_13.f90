module example_mod
   implicit none
contains
   subroutine check_arg(arg)
      class(*), dimension(..), intent(in) :: arg

      select rank (assoc => arg)
       rank (0)
         select type (assoc)
          type is (integer)
            print *, "Received a scalar integer: ", assoc
          class default
            error stop
         end select
       rank (1)
         select type (assoc)
          type is (integer)
            print *, "Received an array of integers (rank: 1)"
            ! If needed, access as 1D: print *, assoc
          class default
            error stop
         end select
       rank (2)
         select type (assoc)
          type is (integer)
            print *, "Received an array of integers (rank: 2)"
            ! If needed, access as 2D: print *, assoc
          class default
            error stop
         end select
       rank default
         print *, "Unsupported array rank: ", rank(arg)
         error stop
      end select
   end subroutine check_arg
end module example_mod

program test
   use example_mod
   implicit none
   integer :: scalar_int = 42
   integer :: int_array1(3) = [1, 2, 3]
   integer :: int_array2(2,2) = reshape([1,2,3,4], [2,2])

   call check_arg(scalar_int)    ! Outputs: Received a scalar integer: 42
   call check_arg(int_array1)    ! Outputs: Received an array of integers (rank: 1)
   call check_arg(int_array2)    ! Outputs: Received an array of integers (rank: 2)
end program test
