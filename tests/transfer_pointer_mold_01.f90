program transfer_pointer_mold_01
  implicit none
  type :: mytype
    integer, pointer :: i(:) => null()
  end type
  type(mytype) :: t
  character, pointer :: ptr(:)
  print *, size(transfer(1, t%i))
  allocate(ptr(9))
  ptr = transfer('Sample#0'//achar(0), ptr)
end program
