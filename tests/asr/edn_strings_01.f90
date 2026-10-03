! Inputs for tests/asr/check_edn.py. The character constants below carry bytes
! that have no text spelling: 0xC8 on its own is not valid UTF-8, and the
! control characters have no portable string escape. Printing them raw would
! produce a document no EDN reader can decode, so the writer falls back to
! #asr/bytes for them. Keep them here so that stays true.
program edn_strings_01
    character(len=1), parameter :: high_byte = achar(200)
    character(len=2), parameter :: control = achar(8)//achar(12)
    character(len=5), parameter :: plain = "plain"
    character(len=4), parameter :: quoted = 'a"b\'
    print *, high_byte, control, plain, quoted
end program
