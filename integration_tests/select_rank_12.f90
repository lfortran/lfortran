module select_rank_12_mod
  implicit none
contains
  subroutine check_arg(arg)
    class(*), dimension(..), intent(in) :: arg

    select rank (assoc => arg)
      rank (1)
        print *, "Received an array of rank 1"
      rank (2)
        print *, "Received an array of rank 2"
      rank default
        print *, "Received an array of unexpected rank"
        error stop
    end select
  end subroutine check_arg
end module select_rank_12_mod

program select_rank_12
  use select_rank_12_mod
  implicit none
  integer :: int_array1(3) = [1, 2, 3]
  integer :: int_array2(2, 3)

  call check_arg(int_array1)
  call check_arg(int_array2)
end program select_rank_12
