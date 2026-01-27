! Test for modfile serialization with nested derived types and default initialization.
! This tests the fix for a bug where structure constructors in default initialization
! expressions contained direct Struct references to types from external modules that
! weren't explicitly imported. When serializing the modfile, these external Struct
! references caused assertion failures during deserialization because the symtab IDs
! weren't present in the modfile's symbol table map.

program modules_62
  use modules_62_parser, only: parser_config, parse
  implicit none
  type(parser_config) :: cfg
  integer :: val

  call parse(cfg, val)
  ! Default level is 0, default reset%style is -1
  ! So val should be 0 + (-1) = -1
  if (val /= -1) error stop
  print *, "PASSED: modules_62"
end program modules_62
