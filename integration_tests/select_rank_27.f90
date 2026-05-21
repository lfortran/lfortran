! Test: character string length preserved when passed to
! class(*), optional, dimension(..) parameter
program select_rank_27
  implicit none
  character(len=:), allocatable :: s
  s = "hello"
  call show(s)
contains
  subroutine show(a)
    class(*), optional, intent(in) :: a(..)
    select rank(v => a)
    rank(0)
      select type(aa => v)
      type is (character(len=*))
        if (len(aa) /= 5) error stop
        if (aa /= "hello") error stop
      class default
        error stop
      end select
    rank default
      error stop
    end select
  end subroutine
end program
