subroutine c()
end subroutine c

module m
contains

  subroutine b()
    call c()
  end subroutine b

end module m
