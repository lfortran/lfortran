program p
  implicit none
  character :: c(1)
  c(1) = 'A'
  
  call s(c)
  
contains

  subroutine s(a)
    character, intent(in) :: a(..)

    select rank(a)
    rank(1)
      print *, a(1)
    end select
  end subroutine s
  
end program p
