
/*
SUMMARY
Win32 applications that run on Windows NT can programmatically eject
media from drives that have hardware support for media removal.
However, they must do so correctly to avoid corrupting the media.
This article explains how to eject media correctly on Windows NT.

MORE INFORMATION
Windows NT version 4.0 and later support ejecting media formatted with
NTFS and FAT file systems without shutting down the machine. Windows
NT 3.51 and earlier support the ejecting FAT formatted media without
shutting down. However, Windows NT versions 3.51 and earlier do not
support removing media formatted with NTFS while the system is running.
On these versions 3.51 and earlier the system must be shut down in order
to avoid corrupting data on the media.

When a volume is mounted on Windows NT, there are two categories of
read and write operations: 1) data operations performed by applications,
and 2) file-system structure related operations performed by Windows NT.
The second category is used by Windows NT to maintain the file system
itself, such as directory entries of files (for example, file times,
sizes, names, etc.).

Win32 applications can use either cached or uncached access to files.
Windows NT, on the other hand, caches some write operations to
file-system data structures to implement a "lazy-writer" scheme. This
allows Windows NT to defer some writes to disk until they are absolutely
required. This way, only the latest changes need to be written to disk if
the file system data is updated often.

Because Windows NT uses a lazy writer system to update file system data
structures on media, the media will be corrupted if the media is ejected
while the system is updating this information. To avoid this problem,
Win32 applications must take the following steps to correctly eject
removable media and prevent possible data corruption:

Call CreateFile with GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE,
and OPEN_EXISTING. The lpFileName parameter should be \\.\X:
(where X is the real drive letter). All other parameters can be zero.

Lock the volume by issuing the FSCTL_LOCK_VOLUME IOCTL via DeviceIoControl.
If any other application or the system is using the volume, this IOCTL
fails. Once this function returns successfully, the application is guaranteed
that the volume is not used by anything else in the system.

Dismount the volume by issuing the FSCTL_DISMOUNT_VOLUME IOCTL. This causes
the file system to remove all knowledge of the volume and to discard any
internal information that it keeps regarding the volume.

Make sure the media can be removed by issuing the IOCTL_STORAGE_MEDIA_REMOVAL IOCTL.
Set the PreventMediaRemoval member of the PREVENT_MEDIA_REMOVAL structure to
FALSE before calling this IOCTL. This stops the device from preventing the
removal of the media.

Eject the media with the IOCTL_STORAGE_EJECT_MEDIA IOCTL. If the device
doesn't allow automatic ejection, then IOCTL_STORAGE_EJECT_MEDIA can be skipped
and the user can be instructed to remove the media.

Close the volume handle obtained in the first step or issue the
FSCTL_UNLOCK_VOLUME IOCTL. This allows the drive to be used by other
processes.

The following code demonstrates how to accomplish safe ejection using the steps
described above:
*/

#include <windows.h>
#include <winioctl.h>
#include <tchar.h>
#include <stdio.h>

#define LOCK_TIMEOUT        5000       // 10 Seconds
#define LOCK_RETRIES        10

// Prototypes

HANDLE OpenVolume          (TCHAR cDriveLetter);
BOOL LockVolume            (HANDLE hVolume);
BOOL DismountVolume        (HANDLE hVolume);
BOOL PreventRemovalOfVolume(HANDLE hVolume, BOOL fPrevent);
BOOL AutoEjectVolume       (HANDLE hVolume);
BOOL CloseVolume           (HANDLE hVolume);

LPTSTR szVolumeFormat = TEXT("\\\\.\\%c:");
LPTSTR szRootFormat   = TEXT("%c:\\");
LPTSTR szErrorFormat  = TEXT("Error %d: %s\n");

//-------------------------------------------------------------------

int WINAPI EjectMediaNt(char cDriveLetter)
  {
  HANDLE hVolume;
  BOOL fRemoveSafely = FALSE;
  BOOL fAutoEject = FALSE;
  // Open the volume.
  hVolume = OpenVolume(cDriveLetter);
  if (hVolume == INVALID_HANDLE_VALUE) return 1;

  // Lock and dismount the volume.
  if (LockVolume(hVolume) && DismountVolume(hVolume))
    {
    fRemoveSafely = TRUE;

    // Set prevent removal to false and eject the volume.
    if (PreventRemovalOfVolume(hVolume, FALSE) &&
         AutoEjectVolume(hVolume))
         fAutoEject = TRUE;
    }

  // Close the volume so other processes can use the drive.
  if (!CloseVolume(hVolume)) return 2;

  if (fAutoEject)
    {
    //printf("Media in Drive %c has been ejected safely.\n", cDriveLetter);
    }
  else
    {
    if (fRemoveSafely)
      {
      //printf("Media in Drive %c can be safely removed.\n", cDriveLetter);
      return 3;
      }
    }
  return 0;
  }

//-------------------------------------------------------------------

HANDLE OpenVolume(TCHAR cDriveLetter)
  {
  HANDLE hVolume;
  UINT uDriveType;
  TCHAR szVolumeName[8];
  TCHAR szRootName[5];
  DWORD dwAccessFlags;

  wsprintf(szRootName, szRootFormat, cDriveLetter);

  uDriveType = GetDriveType(szRootName);
  switch(uDriveType)
    {
    case DRIVE_REMOVABLE:
      dwAccessFlags = GENERIC_READ | GENERIC_WRITE;
      break;
    case DRIVE_CDROM:
      dwAccessFlags = GENERIC_READ;
      break;
    default:
      //_tprintf(TEXT("Cannot eject.  Drive type is incorrect.\n"));
      return INVALID_HANDLE_VALUE;
    }

  wsprintf(szVolumeName, szVolumeFormat, cDriveLetter);

  hVolume = CreateFile(szVolumeName,
                       dwAccessFlags,
                       FILE_SHARE_READ | FILE_SHARE_WRITE,
                       NULL,
                       OPEN_EXISTING,
                       0,
                       NULL );
  return hVolume;
  }

//-------------------------------------------------------------------

BOOL CloseVolume(HANDLE hVolume)
  {
  return CloseHandle(hVolume);
  }

//-------------------------------------------------------------------


BOOL LockVolume(HANDLE hVolume)
  {
  DWORD dwBytesReturned;
  DWORD dwSleepAmount;
  int nTryCount;

  dwSleepAmount = LOCK_TIMEOUT / LOCK_RETRIES;

  // Do this in a loop until a timeout period has expired
  for (nTryCount = 0; nTryCount < LOCK_RETRIES; nTryCount++)
    {
    if (DeviceIoControl(hVolume,
                         FSCTL_LOCK_VOLUME,
                         NULL, 0,
                         NULL, 0,
                         &dwBytesReturned,
                         NULL))
    return TRUE;
    Sleep(dwSleepAmount);
    }
  return FALSE;
  }

//-------------------------------------------------------------------

BOOL DismountVolume(HANDLE hVolume)
  {
  DWORD dwBytesReturned;
  return DeviceIoControl(hVolume,
                         FSCTL_DISMOUNT_VOLUME,
                         NULL, 0,
                         NULL, 0,
                         &dwBytesReturned,
                         NULL);
  }

//-------------------------------------------------------------------

BOOL PreventRemovalOfVolume(HANDLE hVolume, BOOL fPreventRemoval)
  {
  DWORD dwBytesReturned;
  PREVENT_MEDIA_REMOVAL PMRBuffer;
  PMRBuffer.PreventMediaRemoval = fPreventRemoval;
  return DeviceIoControl(hVolume,
                         IOCTL_STORAGE_MEDIA_REMOVAL,
                         &PMRBuffer, sizeof(PREVENT_MEDIA_REMOVAL),
                         NULL, 0,
                         &dwBytesReturned,
                         NULL);
  }

//-------------------------------------------------------------------

AutoEjectVolume(HANDLE hVolume)
  {
  DWORD dwBytesReturned;
  return DeviceIoControl(hVolume,
                         IOCTL_STORAGE_EJECT_MEDIA,
                         NULL, 0,
                         NULL, 0,
                         &dwBytesReturned,
                         NULL);
  }

//-------------------------------------------------------------------




