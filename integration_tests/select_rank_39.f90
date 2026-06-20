program select_rank_39
  implicit none

  character :: letters(1)

  letters = ["a"]
  call check_rank(letters)
  print *, "test passed"
contains

  subroutine check_rank(values)
    character, intent(in) :: values(..)

    select rank(values)
    rank(1)
      if (values(1) /= "a") error stop
    rank default
      error stop
    end select
  end subroutine check_rank

end program select_rank_39
