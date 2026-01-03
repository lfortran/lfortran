module modules_61_mod1
  integer :: inn = 10
end module 

module modules_61_mod2
  integer :: inn = 91
end module 

module modules_61_mod3
  contains
subroutine foo
  use modules_61_mod2
  print *, inn
  if (inn /= 91) error stop
  inn = 1000
  print *, inn
end subroutine

subroutine foo2
  use modules_61_mod1
  print *, inn
  if(inn /= 10) error stop
end subroutine
  
end module

program modules_61
  use modules_61_mod3
  call foo
  call foo2
end program modules_61
