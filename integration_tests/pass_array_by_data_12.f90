module pass_array_by_data_12_mod
  implicit none
  type :: downloader_t
  contains
    procedure, nopass :: upload_form
  end type downloader_t
contains
  subroutine upload_form(form_data)
    integer, intent(inout) :: form_data(:)
    integer :: i
    do i = 1, size(form_data)
        form_data(i) = i
    end do
  end subroutine upload_form
end module pass_array_by_data_12_mod


program pass_array_by_data_12
  use pass_array_by_data_12_mod
  implicit none
  type(downloader_t) :: d
  integer :: arr(5)
  call d%upload_form(arr)
  if (any(arr /= [1,2,3,4,5])) error stop
end program pass_array_by_data_12