program class_78
   integer, parameter :: tfc = selected_char_kind('DEFAULT')
   type :: toml_char_class_78
      character(1, tfc) :: hash = tfc_"#"
   end type toml_char_class_78
   type(toml_char_class_78) :: char_kind = toml_char_class_78()
   character(1, tfc) :: expected
   expected = tfc_"#"
   print *, char_kind%hash
   if (char_kind%hash /= expected) error stop
end program class_78

