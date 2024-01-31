PROGRAM open_notimplemented_kwargs
    implicit none
    ! "action" isn't supported yet, once implemented this test file
    ! (and other reference files) can be safely deleted
    open(10, file='file_01_data.txt', status='old', action='read')
END PROGRAM
