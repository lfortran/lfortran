module entrymod
  implicit none
contains
  function firstname(n)
    integer, intent(in) :: n
    character(24) :: firstname, entryname
    write(firstname, "(A,I0)") "via function name: ", n
    return
    entry entryname(n)
    write(entryname, "(A,I0)") "via entry: ", n
  end function firstname
end module entrymod

program entry_14
  use entrymod
  implicit none
  character(24) :: res
  res = entryname(42)
  if (trim(res) /= "via entry: 42") error stop
  res = firstname(666)
  if (trim(res) /= "via function name: 666") error stop
end program
