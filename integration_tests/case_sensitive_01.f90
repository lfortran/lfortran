module hello_m
  implicit none
  private
  public :: say_hello
  
  interface say_hello
    module procedure Say_Hello_To
  end interface
contains
  subroutine say_hello_to(name, x)
    character(len=*), intent(in) :: name
    integer :: x
    if ( x /= 1023 ) error stop
    print *, "Hello, " // name // "!"
  end subroutine
end module

program test
  use hello_m
  implicit none
  call say_hello("World", 1023)
end program