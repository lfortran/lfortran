module class_86_mod
  type :: downloader_t
        integer, allocatable :: arr(:)
    contains
        procedure, nopass :: upload_form
  end type downloader_t

contains

 subroutine upload_form(strs)
        integer, dimension(:), intent(in) :: strs
    end subroutine upload_form

  subroutine get_from_registry(downloader_)
    class(downloader_t), intent(inout) :: downloader_
    integer :: i
    allocate(downloader_%arr(10))
    downloader_%arr = [(i, i=1,10)]
  end subroutine get_from_registry

end module class_86_mod


program class_86
  use class_86_mod
  implicit none
  type(downloader_t) :: dl
  integer :: i
  call get_from_registry(dl)
  if (any(dl%arr /= [(i, i=1,10)])) error stop
end program class_86
