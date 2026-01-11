program character_12
    use iso_c_binding, only: c_char
    implicit none
    character(:), allocatable :: text
    character(kind=c_char, len=:), allocatable :: y

    text = "Hello"
    if (.not. expect_h(text)) error stop
    if (.not. expect_h("Hello")) error stop
    if (expect_h("World")) error stop
    
    y = 'Hello' // achar(0)
    if (.not. temp(y)) error stop

contains

    logical function expect_h(x)
        character, intent(in) :: x(*)
        expect_h = (x(1) == 'H')
    end function expect_h

    logical function temp(x)
      character(kind=c_char), intent(in) :: x(*)
      integer :: i
      i = 1
      temp = .false.
      if (ichar(x(i)) >= ichar('A') .and. ichar(x(i)) <= ichar('Z')) then
         temp = .true.
      end if
   end function temp

end program character_12

