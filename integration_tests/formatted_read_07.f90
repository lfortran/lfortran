module formatted_read_07_module
  implicit none
  character(*),parameter:: progname='formatted_read_07_text', valfile=progname//'.txt'
  integer, save:: uval !, uin, uout, i, io, nlines, nblanks
  character(:), allocatable:: defaults(:), names(:) ! allocated in main line
  character(:), allocatable:: values(:) ! allocated in beginconsts
  character                ::  msg*200
contains
  subroutine beginconsts(     names,   defaults)
    character(*),intent(in):: names(:),defaults(:)
    integer:: ios,i
    values = defaults(:) ! automatic allocation
    open(newunit=uval,file=valfile)
    read(uval,"(A)",iostat=ios,iomsg=msg) values
    if (ios/=0) error stop trim(msg)
    print "(*(1X,A))", (names(i), defaults(i), i = 1,size(names))
  end subroutine beginconsts
end module formatted_read_07_module
  
program formatted_read_07
  use formatted_read_07_module, only: values, defaults, names, beginconsts
  implicit none
  names = ['INPUT  ','NBLANKS','NLINES '] ! automatic allocation
  defaults = [character(8):: 'filename', '0', '90'] ! automatic allocation

  open(unit=10, file="formatted_read_07_text.txt", status="replace")
  write(10, "(A)") "filename"
  write(10, "(A)") "0"
  write(10, "(A)") "90"
  close(10)
  call beginconsts(names,defaults)
end program formatted_read_07

