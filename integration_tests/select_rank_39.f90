program select_rank_39
  implicit none
  character :: c(1)
  c(1) = 'A'
  
  call s(c)
  
contains

  subroutine s(a)
    character, intent(in) :: a(..)

    if (len(a) /= 1) error stop

    select rank(a)
    rank(1)
      if (a(1) /= 'A') error stop
    end select
  end subroutine s
  
end program select_rank_39