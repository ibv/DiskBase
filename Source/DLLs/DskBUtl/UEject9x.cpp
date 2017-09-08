//---------------------------------------------------------------------------
/*
HOWTO: Eject Removable Media on Windows 95

--------------------------------------------------------------------------------
The information in this article applies to:

Microsoft Win32 Application Programming Interface (API), included with:
Microsoft Windows 95

--------------------------------------------------------------------------------


SUMMARY
Some Win32 applications need to have the ability to eject removable media
from a drive on Windows 95. To do so safely, an application should first make
sure that no other applications are currently using the media. Ejecting the
media while another application has files open on the media could lead to
those files becoming corrupt or, at the very least, the user could lose data.
This article explains how to safely eject removable media on Windows 95, and
presents example code that demonstrates this.

MORE INFORMATION
An application can safely eject removable media by first making sure that
no files on the media are being used. Once this is done, the application
can eject the media. On Windows 95, there are no special "dismount volume"
functions that need to be called before removing the media.

Because removable media are used in drives that have an associated drive
letter, all operations will be targeted to the drive rather than the
specific media in the drive. For example, to eject the media in drive E:,
an application sends the media removal request to drive E:.

The steps to ejecting the media safely are:

Lock the volume for exclusive access with Int 21h function 440Dh minor code 4Ah.
Unlock the media so that it can be removed with Int 21h function 440Dh minor code 48h.
Eject the media with Int 21h function 440Dh minor code 49h.
Remove the exclusive volume lock taken in step 1 with Int 21h function 440Dh minor code 6Ah.

Win32 applications may issue the requests in all of these steps by using
DeviceIoControl to send the Int 21h IOCTL requests to VWIN32. The following
sample code demonstrates how this is done.

Sample Code
*/

#include <windows.h>
#include <stdio.h>
#include <ctype.h>

//-----------------------------------------------------------------------
// DeviceIoControl infrastructure

#if !defined (VWIN32_DIOC_DOS_IOCTL)
#define VWIN32_DIOC_DOS_IOCTL      1

typedef struct _DIOC_REGISTERS
  {
  DWORD reg_EBX;
  DWORD reg_EDX;
  DWORD reg_ECX;
  DWORD reg_EAX;
  DWORD reg_EDI;
  DWORD reg_ESI;
  DWORD reg_Flags;
  } DIOC_REGISTERS, *PDIOC_REGISTERS;

#endif

// Intel x86 processor status flags
#define CARRY_FLAG             0x0001

//-----------------------------------------------------------------------
// DOS IOCTL function support
#pragma pack(1)

// Parameters for locking/unlocking removable media
typedef struct _PARAMBLOCK
  {
  BYTE bOperation;
  BYTE bNumLocks;
  } PARAMBLOCK, *PPARAMBLOCK;

#pragma pack()

//-----------------------------------------------------------------------
// Win95 low-level media unlocking/removal support

HANDLE WINAPI OpenVWin32  (void);
BOOL   WINAPI CloseVWin32 (HANDLE hVWin32);
BOOL   WINAPI UnlockLogicalVolume (HANDLE hVWin32, BYTE bDriveNum);
BOOL   WINAPI LockLogicalVolume   (HANDLE hVWin32,
                                   BYTE   bDriveNum,
                                   BYTE   bLockLevel,
                                   WORD   wPermissions);
BOOL   UnlockMedia (HANDLE hVWin32, BYTE bDrive);
BOOL   EjectMedia  (HANDLE hVWin32, BYTE bDrive);

//-----------------------------------------------------------------------

int WINAPI EjectMedia9x (char cDriveLetter)
  {
  HANDLE hVWin32      = INVALID_HANDLE_VALUE;
  BYTE   bDrive;
  BOOL   fDriveLocked = FALSE;
  int    iResult = 0;

  // convert command line arg 1 from a drive letter to a DOS drive
  // number
  bDrive = (toupper (cDriveLetter) - 'A') + 1;
  hVWin32 = OpenVWin32 ();

  // Make sure no other applications are using the drive.
  fDriveLocked = LockLogicalVolume (hVWin32, bDrive, 0, 0);
  if (!fDriveLocked)
    {
    iResult = 1;
    //printf("volume %c is in use by another application; therefore, it "
    //       "cannot be ejected\n", 'A' + bDrive - 1);
    goto CLEANUP_AND_EXIT_APP;
    }

  // Make sure there is no software lock keeping the media in the drive.
  if (!UnlockMedia (hVWin32, bDrive))
    {
    iResult = 2;
    //printf("could not unlock media from drive %c:\n", 'A' + bDrive - 1);
    goto CLEANUP_AND_EXIT_APP;
    }

  // Eject the media
  if (!EjectMedia (hVWin32, bDrive))
    {
    iResult = 3;
    goto CLEANUP_AND_EXIT_APP;
    //printf("could not eject media from drive %c:\n", 'A' + bDrive - 1);
    }

  CLEANUP_AND_EXIT_APP:
  if (fDriveLocked) UnlockLogicalVolume (hVWin32, bDrive);
  if (hVWin32 != INVALID_HANDLE_VALUE) CloseVWin32 (hVWin32);
  return iResult;
  }

/*-----------------------------------------------------------------------
UnlockMedia (hVWin32, bDrive)

Purpose:
   Unlocks removable media from the specified drive so that it can be
   ejected.

Parameters:
   hVWin32
      A handle to VWIN32. Used to issue request to unlock the media.

   bDrive
      The logical drive number to unlock. 0 = default, 1 = A, 2 = B,
      etc.

Return Value:
   If successful, returns TRUE; if unsuccessful, returns FALSE.
-----------------------------------------------------------------------*/

BOOL UnlockMedia (HANDLE hVWin32, BYTE bDrive)
  {
  DIOC_REGISTERS regs = {0};
  PARAMBLOCK     unlockParams = {0};
  int   i;
  BOOL  fResult;
  DWORD cb;

  // First, check the lock status. This way, you'll know the number of
  // pending locks you must unlock.

  unlockParams.bOperation = 2;   // return lock/unlock status

  regs.reg_EAX = 0x440D;
  regs.reg_EBX = bDrive;
  regs.reg_ECX = MAKEWORD(0x48, 0x08);
  regs.reg_EDX = (DWORD)&unlockParams;

  fResult = DeviceIoControl (hVWin32, VWIN32_DIOC_DOS_IOCTL,
                            &regs, sizeof(regs), &regs, sizeof(regs),
                            &cb, 0);

  // See if DeviceIoControl and the unlock succeeded.
  if (fResult)
    {
    /*
    DeviceIoControl succeeded. Now see if the unlock succeeded. It
    succeeded if the carry flag is not set, or if the carry flag is
    set but EAX is 0x01 or 0xB0.

    It failed if the carry flag is set and EAX is not 0x01 or 0xB0.

    If the carry flag is clear, then unlock succeeded. However, you
    don't need to set fResult because it is already TRUE when you get
    in here.
    */
    if (regs.reg_Flags & CARRY_FLAG)
       fResult = (regs.reg_EAX == 0xB0) || (regs.reg_EAX == 0x01);
    }

  if (!fResult) return (FALSE);

  // Now, let's unlock the media for every time it has been locked;
  // this will totally unlock the media.
  for (i = 0; i < unlockParams.bNumLocks; ++i)
    {
    unlockParams.bOperation = 1;   // unlock the media
    regs.reg_EAX = 0x440D;
    regs.reg_EBX = bDrive;
    regs.reg_ECX = MAKEWORD(0x48, 0x08);
    regs.reg_EDX = (DWORD)&unlockParams;

    fResult = DeviceIoControl (hVWin32, VWIN32_DIOC_DOS_IOCTL,
                               &regs, sizeof(regs), &regs, sizeof(regs),
                               &cb, 0);

    // See if DeviceIoControl and the lock succeeded
    fResult = fResult && !(regs.reg_Flags & CARRY_FLAG);
    if (!fResult) break;
    }
  return fResult;
  }

/*-----------------------------------------------------------------------
EjectMedia (hVWin32, bDrive)

Purpose:
   Ejects removable media from the specified drive.

Parameters:
   hVWin32
      A handle to VWIN32. Used to issue request to unlock the media.

   bDrive
      The logical drive number to unlock. 0 = default, 1 = A, 2 = B,
      etc.

Return Value:
   If successful, returns TRUE; if unsuccessful, returns FALSE.
-----------------------------------------------------------------------*/

BOOL EjectMedia (HANDLE hVWin32, BYTE bDrive)
  {
  DIOC_REGISTERS regs = {0};
  BOOL  fResult;
  DWORD cb;

  regs.reg_EAX = 0x440D;
  regs.reg_EBX = bDrive;
  regs.reg_ECX = MAKEWORD(0x49, 0x08);

  fResult = DeviceIoControl (hVWin32, VWIN32_DIOC_DOS_IOCTL,
                            &regs, sizeof(regs), &regs, sizeof(regs),
                            &cb, 0);

  // See if DeviceIoControl and the lock succeeded
  fResult = fResult && !(regs.reg_Flags & CARRY_FLAG);

  return fResult;
  }

/*-----------------------------------------------------------------------
OpenVWin32 ()

Purpose:
   Opens a handle to VWIN32 that can be used to issue low-level disk I/O
   commands.

Parameters:
   None.

Return Value:
   If successful, returns a handle to VWIN32.

   If unsuccessful, return INVALID_HANDLE_VALUE. Call GetLastError() to
   determine the cause of failure.
-----------------------------------------------------------------------*/

HANDLE WINAPI OpenVWin32 (void)
  {
  return CreateFile ("\\\\.\\vwin32", 0, 0, NULL, 0,
                      FILE_FLAG_DELETE_ON_CLOSE, NULL);
  }

/*-----------------------------------------------------------------------
CloseVWin32 (hVWin32)

Purpose:
   Closes the handle opened by OpenVWin32.

Parameters:
   hVWin32
      An open handle to VWIN32.

Return Value:
   If successful, returns TRUE. If unsuccessful, returns FALSE. Call
   GetLastError() to determine the cause of failure.
-----------------------------------------------------------------------*/

BOOL WINAPI CloseVWin32 (HANDLE hVWin32)
  {
  return CloseHandle (hVWin32);
  }

/*-----------------------------------------------------------------------
LockLogicalVolume (hVWin32, bDriveNum, bLockLevel, wPermissions)

Purpose:
   Takes a logical volume lock on a logical volume.

Parameters:
   hVWin32
      An open handle to VWIN32.

   bDriveNum
      The logical drive number to lock. 0 = default, 1 = A:, 2 = B:,
      3 = C:, etc.

   bLockLevel
      Can be 0, 1, 2, or 3. Level 0 is an exclusive lock that can only
      be taken when there are no open files on the specified drive.
      Levels 1 through 3 form a hierarchy where 1 must be taken before
      2, which must be taken before 3.

   wPermissions
      Specifies how the lock will affect file operations when lock levels
      1 through 3 are taken. Also specifies whether a formatting lock
      should be taken after a level 0 lock.

      Zero is a valid permission.

Return Value:
   If successful, returns TRUE.  If unsuccessful, returns FALSE.
-----------------------------------------------------------------------*/

BOOL WINAPI LockLogicalVolume (HANDLE hVWin32,
                               BYTE   bDriveNum,
                               BYTE   bLockLevel,
                               WORD   wPermissions)
  {
  BOOL           fResult;
  DIOC_REGISTERS regs = {0};
  BYTE           bDeviceCat;  // can be either 0x48 or 0x08
  DWORD          cb;

  /*
    Try first with device category 0x48 for FAT32 volumes. If it
    doesn't work, try again with device category 0x08. If that
    doesn't work, then the lock failed.
  */

  bDeviceCat = 0x48;

  ATTEMPT_AGAIN:
  // Set up the parameters for the call.
  regs.reg_EAX = 0x440D;
  regs.reg_EBX = MAKEWORD(bDriveNum, bLockLevel);
  regs.reg_ECX = MAKEWORD(0x4A, bDeviceCat);
  regs.reg_EDX = wPermissions;

  fResult = DeviceIoControl (hVWin32, VWIN32_DIOC_DOS_IOCTL,
                            &regs, sizeof(regs), &regs, sizeof(regs),
                            &cb, 0);

  // See if DeviceIoControl and the lock succeeded
  fResult = fResult && !(regs.reg_Flags & CARRY_FLAG);

  // If DeviceIoControl or the lock failed, and device category 0x08
  // hasn't been tried, retry the operation with device category 0x08.
  if (!fResult && (bDeviceCat != 0x08))
    {
    bDeviceCat = 0x08;
    goto ATTEMPT_AGAIN;
    }

  return fResult;
  }

/*-----------------------------------------------------------------------
UnlockLogicalVolume (hVWin32, bDriveNum)

Purpose:
   Unlocks a logical volume that was locked with LockLogicalVolume().

Parameters:
   hVWin32
      An open handle to VWIN32.

   bDriveNum
      The logical drive number to unlock. 0 = default, 1 = A:, 2 = B:,
      3 = C:, etc.

Return Value:
   If successful, returns TRUE. If unsuccessful, returns FALSE.

Comments:
   Must be called the same number of times as LockLogicalVolume() to
   completely unlock a volume.

   Only the lock owner can unlock a volume.
-----------------------------------------------------------------------*/

BOOL WINAPI UnlockLogicalVolume (HANDLE hVWin32, BYTE bDriveNum)
  {
  BOOL           fResult;
  DIOC_REGISTERS regs = {0};
  BYTE           bDeviceCat;  // can be either 0x48 or 0x08
  DWORD          cb;

  // Try first with device category 0x48 for FAT32 volumes. If it
  //   doesn't work, try again with device category 0x08. If that
  //   doesn't work, then the unlock failed.

  bDeviceCat = 0x48;

  ATTEMPT_AGAIN:
  // Set up the parameters for the call.
  regs.reg_EAX = 0x440D;
  regs.reg_EBX = bDriveNum;
  regs.reg_ECX = MAKEWORD(0x6A, bDeviceCat);

  fResult = DeviceIoControl (hVWin32, VWIN32_DIOC_DOS_IOCTL,
                            &regs, sizeof(regs), &regs, sizeof(regs),
                            &cb, 0);

  // See if DeviceIoControl and the unlock succeeded
  fResult = fResult && !(regs.reg_Flags & CARRY_FLAG);

  // If DeviceIoControl or the unlock failed, and device category 0x08
  // hasn't been tried, retry the operation with device category 0x08.
  if (!fResult && (bDeviceCat != 0x08))
    {
    bDeviceCat = 0x08;
    goto ATTEMPT_AGAIN;
    }
  return fResult;
  }


/*
REFERENCES
For more information about the Int 21h IOCTL calls used in this article
and the Windows 95 DeviceIoControl mechanism, please see the
Programmer's Guide to Windows 95 in the Win32 SDK documentation.
*/
