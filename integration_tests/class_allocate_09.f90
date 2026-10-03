program class_allocate_09

  type base_t
    integer, allocatable :: buf(:)
  end type base_t

  type holder_t
    class(base_t), allocatable :: obj(:)
  end type holder_t

  type(holder_t) :: x

  allocate(x%obj(2))
  deallocate(x%obj)

  print *, "test passed"

end program class_allocate_09
