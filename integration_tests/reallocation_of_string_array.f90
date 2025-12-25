module m_dep
  type :: downloader_t
     character(len=:), allocatable :: name
    end type downloader_t
end module m_dep


program reallocation
  use m_dep
  type(downloader_t), allocatable :: dl(:)
    integer :: i
        i = 1
  allocate(dl(2))
    dl(1)%name = "first"
    dl(2)%name = "second"
    dl = dl(1:i)
    print *, allocated(dl(1)%name)
end program reallocation