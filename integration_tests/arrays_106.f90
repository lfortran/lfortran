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

program array_106
  use tbp_mod
  implicit none

  type(tbp) :: b(3)

  b%i = [1,2,3]

 if (any(b%i /= [1,2,3])) error stop
 print *, "test passed"

end program array_106
