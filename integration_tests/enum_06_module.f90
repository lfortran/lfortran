module enum_06_mod_main
    implicit none
    enum, bind(C)
      enumerator :: red = 100
    end enum
end module
  

module enum_06_mod_middle
    use enum_06_mod_main
end module  