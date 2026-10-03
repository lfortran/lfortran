program bindc_51
  implicit none
  call show_letter('z')
contains

  subroutine show_letter(ch) bind(c)
    character, value :: ch
    if (ch /= 'z') error stop
    print *, ch
  end subroutine

end program
