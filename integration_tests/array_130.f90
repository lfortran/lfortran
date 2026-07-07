program main
  call f("a")
contains

  subroutine f(mold)
    character(*), intent(in) :: mold

    print *, size(transfer(0.0, [mold]))
  end subroutine f
end program main
