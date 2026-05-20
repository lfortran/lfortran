
! Test the addressed variable declaration below
! It exercises the `function_call_in_declration` pass
module functions_60_mod
  contains
  pure function foo() result(r)
    integer, allocatable :: r(:)
    allocate(r(10))
    r = 1
  end function
end module

program functions_60

  use functions_60_mod
  call test

  contains 
  subroutine test()
    integer :: arr(sum(foo())) !! <<<
    if(size(arr) /= 10) error stop
  end subroutine test

end program
