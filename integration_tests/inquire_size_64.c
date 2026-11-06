#include <stdint.h>

#if defined(_WIN32)
#include <windows.h>
#include <winioctl.h>
#else
#include <fcntl.h>
#include <unistd.h>
#endif

int create_inquire_size_file(int64_t length)
{
#if defined(_WIN32)
    HANDLE file = CreateFileA("inquire_size_64.dat", GENERIC_WRITE, 0, NULL,
        CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) return 1;
    DWORD returned;
    LARGE_INTEGER offset;
    offset.QuadPart = length;
    int failed = !DeviceIoControl(file, FSCTL_SET_SPARSE, NULL, 0, NULL, 0,
        &returned, NULL) || !SetFilePointerEx(file, offset, NULL, FILE_BEGIN)
        || !SetEndOfFile(file);
    CloseHandle(file);
#else
    int file = open("inquire_size_64.dat", O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (file < 0) return 1;
    int failed = ftruncate(file, length) != 0;
    if (close(file) != 0) failed = 1;
#endif
    return failed;
}
