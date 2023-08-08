!test location information for errors
program test
#ifdef SOMETHING
#endif // This should work
  implicit none

  integer :: abc = 10
  /*
  hi*/

  print *, "asdjfal"
  print *, abcx
end program test
