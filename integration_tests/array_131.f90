program array_131
  call f("a")

contains

  subroutine f(mold)
    character(*), intent(in) :: mold
    if (size(transfer(0.0, [mold])) /= 4) error stop
  end subroutine f

end program array_131