program allocate_26
implicit none
character(:), allocatable :: temp_file
allocate(temp_file, source=get_temp_filename())
if (temp_file /= "tmp_12345.txt") error stop

contains
  function get_temp_filename() result(fname)
    character(len=:), allocatable :: fname
    fname = "tmp_12345.txt"
  end function get_temp_filename
end program allocate_26