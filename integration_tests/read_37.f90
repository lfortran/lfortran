program read_struct_internal
  type :: t
     integer :: a
  end type t

  type(t) :: x
  character(len=10) :: buf

  buf = "123"

  ! This used to cause ICE (StructType passed to lowering)
  read(buf, *) x

  if (x%a /= 123) error stop "Struct read failed"

  print *, "OK"
end program read_struct_internal
