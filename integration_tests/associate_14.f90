program associate_14
    character(:), allocatable , target:: char_physically_descriptor 
    character(:), pointer :: char_ptr
    char_physically_descriptor = "hello"
    char_ptr => char_physically_descriptor
  end program associate_14