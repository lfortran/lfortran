module bar_character_29
  implicit none
contains

elemental function trim_append(xx,yy) result(xy)
  character(len=*), intent(in) :: xx,yy
  character(len=len(xx)+len(yy)) :: xy
  xy = trim(xx) // yy
end function

function same(xx) result(yy)
  character(len=*), intent(in) :: xx(:)
  character(len=len(xx)) :: yy(size(xx))
  yy = [xx]
end function

subroutine foo(labels)
  character(len=*), intent(in) :: labels(:)
  print *, "size(labels)=", size(labels)
  if (size(labels) /= 1) error stop
end subroutine

subroutine xmain()
  call foo(trim_append(["a"], same(["b"])))
end subroutine

end module bar_character_29

program character_29
  use bar_character_29
  call xmain()
end program