module tbp_mod 
  implicit none
  type tbp
     integer :: i
   contains
     procedure :: print_1 
  end type tbp
contains
  subroutine print_1(this, arr)
    class(tbp), intent(in) :: this, arr(:)
    print '(a,1x,*(i0,:,","))', 'print_1:', arr%i
  end subroutine print_1
end module tbp_mod

program xxx
  use tbp_mod
  implicit none

  type(tbp) :: b(3)

  b%i = [1,2,3]

  if (all(b%i == [1,2,3])) then
     call b(1)%print_1(b)
     print *, "test passed"
  else
     print *, "test failed"
  end if

end program xxx