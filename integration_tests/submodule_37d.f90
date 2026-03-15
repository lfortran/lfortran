module submodule_37_user
  use submodule_37_mod, only: mytype
  implicit none
contains
  subroutine use_it()
    type(mytype) :: t
    t = mytype("1.0 2.0 3.0")
    print *, t%get_val()
  end subroutine
end module submodule_37_user
