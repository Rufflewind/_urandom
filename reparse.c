// compile with -municode
#include <stdio.h>
#include <windows.h>

typedef struct {
    ULONG ReparseTag;
    USHORT ReparseDataLength;
    USHORT Reserved;
    union {
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            ULONG Flags;
            WCHAR PathBuffer[1];
        } SymbolicLinkReparseBuffer;
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            WCHAR PathBuffer[1];
        } MountPointReparseBuffer;
        struct {
            UCHAR DataBuffer[1];
        } GenericReparseBuffer;
    };
} REPARSE_DATA_BUFFER;

wchar_t *strerr(DWORD dwErrorCode)
{
    wchar_t *msg = L"";
    FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                   FORMAT_MESSAGE_FROM_SYSTEM |
                   FORMAT_MESSAGE_IGNORE_INSERTS,
                   NULL, dwErrorCode,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                   (wchar_t *)&msg, 0, NULL);
    return msg;
}

int wmain(int argc, wchar_t **argv)
{
    (void)argc;
    if (!argv[1]) {
        wprintf(L"error: expected 1 argument\n");
        return 1;
    }
    HANDLE *h = CreateFileW(argv[1], 0, 0, NULL,
                            OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS |
                            FILE_FLAG_OPEN_REPARSE_POINT,
                            NULL);
    if (!h) {
        wprintf(L"CreateFileW: %s", strerr(GetLastError()));
        return 1;
    }
    union {
        REPARSE_DATA_BUFFER rdb;
        char cs[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    } buf;
    if (!DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0,
                         &buf, sizeof(buf), NULL, NULL)) {
        wprintf(L"DeviceIoControl: %s", strerr(GetLastError()));
        return 1;
    };
    if (buf.rdb.ReparseTag == IO_REPARSE_TAG_MOUNT_POINT) {
        wprintf(L"ReparseTag = mount_point\n");
        wprintf(L"PathBuffer = %s\n",
                buf.rdb.MountPointReparseBuffer.PathBuffer);
    } else if (buf.rdb.ReparseTag == IO_REPARSE_TAG_SYMLINK) {
        wprintf(L"ReparseTag = symlink\n");
        wprintf(L"PathBuffer = %s\n",
                buf.rdb.SymbolicLinkReparseBuffer.PathBuffer);
    } else {
        wprintf(L"ReparseTag = %u\n", buf.rdb.ReparseTag);
    }
    CloseHandle(h);
    return 0;
}
