module m_dep
  type :: downloader_t
     character(len=:), allocatable :: name
     integer :: nn
  end type downloader_t
end module m_dep

program demo
  use m_dep
  type(downloader_t), allocatable :: dl(:)
  allocate(dl(2))
  allocate(character(len=1000000) :: dl(2)%name)
  dl = dl(2:2)
  print *, dl(1)%NAME
end program demo
