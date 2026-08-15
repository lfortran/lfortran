module lfortran_display
  use iso_c_binding, only: c_char, c_null_char
  implicit none

  interface
    subroutine lf_display_data(mime, payload) bind(C, name="lfortran_display_data")
      import :: c_char
      character(kind=c_char), intent(in) :: mime(*), payload(*)
    end subroutine

    subroutine lf_clear_output() bind(C, name="lfortran_clear_output")
    end subroutine
  end interface

contains

  ! Generic display: send any MIME type + data to Jupyter
  ! Examples:
  !   call display_data("text/html", "<h1>Hello</h1>")
  !   call display_data("image/svg+xml", svg_string)
  !   call display_data("image/bmp", base64_bmp_string)
  subroutine display_data(mime_type, data)
    character(len=*), intent(in) :: mime_type, data
    call lf_display_data(trim(mime_type)//c_null_char, trim(data)//c_null_char)
  end subroutine

  ! Clear output produced by the current cell
  subroutine clear_output()
    call lf_clear_output()
  end subroutine

end module lfortran_display
