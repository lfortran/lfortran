program read_51
  implicit none
  real :: xnan
  character :: cni*4, msg*200
  integer :: ios
  cni = 'NAN '
  read(cni, *, iostat=ios, iomsg=msg) xnan
  if (ios /= 0) then
    print *, "Reading 'NAN ' into xnan failed."
    print *, 'iomsg = '//trim(msg)
    error stop
  else
    if (.not. (xnan /= xnan)) error stop "NaN should not equal itself"
    print "(A,L2,1X,F0.0)", ' NaN/=NaN?', xnan /= xnan, xnan
  end if
end program read_51
