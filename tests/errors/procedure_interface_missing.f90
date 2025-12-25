module m
  implicit none

  type :: ctx_t
     ! This should trigger an error because 'f_missing' is not defined
     procedure(f_missing), pointer, nopass :: fn => null()
  end type ctx_t

end module m

program test
    use m
end program