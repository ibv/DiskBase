unit UUnAce;
(*====================================================================
ACE files decompression
======================================================================*)

interface

uses
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages, TTTypes ,
  {$ENDIF}
  SysUtils;

////////////////////////////////////////////////////////////////////////
// ACE constants
////////////////////////////////////////////////////////////////////////

const
// Constants for the tACECommentStruc.State field, which tells about
// the result of the last comment extraction.
  ACE_COMMENT_OK          = 0;    // comment extraction went fine
  ACE_COMMENT_SMALLBUF    = 1;    // comment buffer too small to
                                  // store the whole comment in
  ACE_COMMENT_NONE        = 255;  // No comment present


// Flag constants for tACEArchiveDataStruc.Flags field.
  ACE_ARCFLAG_MAINCOMMENT   =       2;
  ACE_ARCFLAG_SFX           =     512;
  ACE_ARCFLAG_LIMITSFXJR    =    1024;  // is an SFX archive
                                       // that supports 256k
                                       // dictionary only
  ACE_ARCFLAG_MULTIVOLUME   =    2048;
  ACE_ARCFLAG_AV            =    4096;  // not used in ACL
  ACE_ARCFLAG_RECOVERYREC   =    8192;
  ACE_ARCFLAG_LOCK          =   16384;
  ACE_ARCFLAG_SOLID         =   32768;

// Host system used to create an archive. Used at
// tACEArchiveDataStruc.HostCreated field.
  ACE_HOST_MSDOS            =       0;   // archive created by
                                         // MSDOS ACE archiver
  ACE_HOST_OS2              =       1;   // created by OS2 ACE
  ACE_HOST_WIN32            =       2;   // created by Win32 ACE



// Flag constants for the tACEFileData.Flags field.
  ACE_FILEFLAG_FILECOMMENT  =      2; // file has comment
  ACE_FILEFLAG_SPLITBEFORE  =   4096; // continued from
                                       // previous volume
  ACE_FILEFLAG_SPLITAFTER   =   8192; // continued on
                                       // next volume
  ACE_FILEFLAG_PASSWORD     =  16384; // is encrypted
  ACE_FILEFLAG_SOLID        =  32768; // uses data of previous
                                       // files (solid mode)


// Tells the Dll which compression level to use. (ACE only)
  ACE_LEVEL_STORE    = 0; // save file only; do not compress
  ACE_LEVEL_FASTEST  = 1; // compress very fast
  ACE_LEVEL_FAST     = 2; // compress fast
  ACE_LEVEL_NORMAL   = 3; // good compromise between speed and
                          // compression rate
  ACE_LEVEL_GOOD     = 4; // achieves good compression
  ACE_LEVEL_BEST     = 5; // best compression; bit slow

////////////////////////////////////////////////////////////////////////
//                          Part 2.1: operation codes
//
// Passed to callback functions indicating the current operation.

  ACE_CALLBACK_OPERATION_LIST         =  0;
  ACE_CALLBACK_OPERATION_TEST         =  1;
  ACE_CALLBACK_OPERATION_ANALYZE      =  2;
  ACE_CALLBACK_OPERATION_EXTRACT      =  3;
  ACE_CALLBACK_OPERATION_ADD          =  4;
  ACE_CALLBACK_OPERATION_REPACK       =  5;
  ACE_CALLBACK_OPERATION_DELETE       =  6;
  ACE_CALLBACK_OPERATION_REPAIR       =  7;    // repair without
                                               // recovery record
  ACE_CALLBACK_OPERATION_SETCMT       =  8;
  ACE_CALLBACK_OPERATION_ENCRYPT      =  9;
  ACE_CALLBACK_OPERATION_KEEP         = 10;    // file is to be
                                               // taken along
                                               // without recompress
  ACE_CALLBACK_OPERATION_RECOVER      = 11;    // begin of
                                               // recovering archive
                                               // by recovery record
  ACE_CALLBACK_OPERATION_HEADSEARCH   = 12;    // begin of searching
                                               // for file headers
  ACE_CALLBACK_OPERATION_RECRECSEARCH = 13;    // begin of searching
                                               // for recovery record
  ACE_CALLBACK_OPERATION_ADDSFX       = 14;
  ACE_CALLBACK_OPERATION_LOCK         = 15;
  ACE_CALLBACK_OPERATION_ADDAV        = 16;    // not used in ACL
  ACE_CALLBACK_OPERATION_ADDRECOVREC  = 17;
  ACE_CALLBACK_OPERATION_REGISTER     = 18;    // not used in ACL


////////////////////////////////////////////////////////////////////////
//                  Part 2.2: callback function return codes
// One of these result codes has to be returned by the application-based
// callback functions.

  ACE_CALLBACK_RETURN_OK      =    0;      // also "yes" at
                                           // requests
  ACE_CALLBACK_RETURN_NO      =    1;      // no, do not/
                                           // do not retry
  ACE_CALLBACK_RETURN_CANCEL  =    2;      // abort operation


////////////////////////////////////////////////////////////////////////
//                     Part 2.3: callback structure types
// States of which type the passed structure is when a callback function
// is called. The type is written to the StructureType field.

  ACE_CALLBACK_TYPE_GLOBAL    =    0;

                // type of structure is
                // tACECallbackGlobalStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // InfoCallbackProc    | ACE_CALLBACK_INFO_GENERALKEY      // not used in ACL
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_MEMORY // fr ManyFilesError und ExtractMem andere Codes verwenden!?
                //                     | ACE_CALLBACK_ERROR_REGISTER       // not used in ACL
                //                     | ACE_CALLBACK_ERROR_READKEY        // not used in ACL
                //                     | ACE_CALLBACK_ERROR_WRITEKEY       // not used in ACL
                //                     | ACE_CALLBACK_ERROR_NOWINACEKEY    // not used in ACL
                //                     | ACE_CALLBACK_ERROR_NOACTIVEACEKEY // not used in ACL
                //                     | ACE_CALLBACK_ERROR_UNCSPACE       // wird noch nicht verwendet!
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // RequestCallbackProc | ACE_CALLBACK_REQUEST_REGISTER     // not used in ACL
                //


  ACE_CALLBACK_TYPE_ARCHIVE = 1;

                // type of structure is
                // tACECallbackArchiveStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // InfoCallbackProc    | ACE_CALLBACK_INFO_TMPARCCREATE
                //                     | ACE_CALLBACK_INFO_TMPARCCREATEEND
                //                     | ACE_CALLBACK_INFO_ADDRECREC
                //                     | ACE_CALLBACK_INFO_ADDRECRECEND
                //                     | ACE_CALLBACK_INFO_RECREC
                //                     | ACE_CALLBACK_INFO_NORECREC
                //                     | ACE_CALLBACK_INFO_RECOVERED
                //                     | ACE_CALLBACK_INFO_NODAMAGE
                //                     | ACE_CALLBACK_INFO_FNDMAINHEAD
                //                     | ACE_CALLBACK_INFO_FILELISTCREATE
                //                     | ACE_CALLBACK_INFO_FILELISTCREATEEND
                //                     | ACE_CALLBACK_INFO_FILESORT
                //                     | ACE_CALLBACK_INFO_FILESORTEND
                //                     | ACE_CALLBACK_INFO_COPYEND
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_MODIFYVOLUME
                //                     | ACE_CALLBACK_ERROR_MODIFYLOCKEDARCHIVE
                //                     | ACE_CALLBACK_ERROR_AV                  // not used in ACL
                //                     | ACE_CALLBACK_ERROR_TOODAMAGED
                //                     | ACE_CALLBACK_ERROR_ARCHIVEEXISTS
                //                     | ACE_CALLBACK_ERROR_OPENREPAIRARCHIVE
                //                     | ACE_CALLBACK_ERROR_OPENARCHIVEREAD
                //                     | ACE_CALLBACK_ERROR_OPENARCHIVEWRITE
                //                     | ACE_CALLBACK_ERROR_READARCHIVE
                //                     | ACE_CALLBACK_ERROR_WRITEARCHIVE
                //                     | ACE_CALLBACK_ERROR_ALREADYSFX
                //                     | ACE_CALLBACK_ERROR_ADDSFXTOVOLUME
                //                     | ACE_CALLBACK_ERROR_ARCHIVEBROKEN
                //                     | ACE_CALLBACK_ERROR_ARCHIVESAVE
                //                     | ACE_CALLBACK_ERROR_NOFILES
                //                     | ACE_CALLBACK_ERROR_ISNOTANARCHIVE
                //                     | ACE_CALLBACK_ERROR_TEMPDIRCREATE
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // RequestCallbackProc | ACE_CALLBACK_REQUEST_MARKASSOLID
                //                     | ACE_CALLBACK_REQUEST_CHANGEVOLUME
                //                     | ACE_CALLBACK_REQUEST_ARCHIVEEXISTS
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // StateCallbackProc   | ACE_CALLBACK_STATE_STARTARCHIVE
                //


  ACE_CALLBACK_TYPE_ARCHIVEDFILE = 2;

                // type of structure is
                // tACECallbackArchivedFileStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // InfoCallbackProc    | ACE_CALLBACK_INFO_TMPARCCREATE
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_CREATIONNAMEINUSE
                //                     | ACE_CALLBACK_ERROR_HIGHERVERSION
                //                     | ACE_CALLBACK_ERROR_ENCRYPTIONCRC
                //                     | ACE_CALLBACK_ERROR_WRITE
                //                     | ACE_CALLBACK_ERROR_READ
                //                     | ACE_CALLBACK_ERROR_OPENREAD
                //                     | ACE_CALLBACK_ERROR_OPENWRITE //wird noch gar nich benutzt?? sollte aber - bei extract!
                //                     | ACE_CALLBACK_ERROR_FILENAMETOOLONG
                //                     | ACE_CALLBACK_ERROR_REPACKCRC
                //                     | ACE_CALLBACK_ERROR_EXCLUDEPATH
                //                     | ACE_CALLBACK_ERROR_METHOD
                //                     | ACE_CALLBACK_ERROR_EXTRACTSPACE
                //                     | ACE_CALLBACK_ERROR_CREATION
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // RequestCallbackProc | ACE_CALLBACK_REQUEST_OVERWRITE
                //                     | ACE_CALLBACK_REQUEST_DELETEARCHIVEDSYSFILE
                //                     | ACE_CALLBACK_REQUEST_ADDBROKENFILE
                //                     | ACE_CALLBACK_REQUEST_PASSWORD
                //                     | ACE_CALLBACK_REQUEST_OVERWRITESYSFILE
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // StateCallbackProc   | ACE_CALLBACK_STATE_STARTFILE
                //                     | ACE_CALLBACK_STATE_ENDNOCRCCHECK
                //


  ACE_CALLBACK_TYPE_REALFILE = 3;

                // type of structure is
                // tACECallbackRealFileStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // InfoCallbackProc    | ACE_CALLBACK_INFO_FILELISTADD
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_MOVEDELETE
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // RequestCallbackProc | ACE_CALLBACK_REQUEST_MOVEDELETEREALSYSFILE
                // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                // StateCallbackProc   | ACE_CALLBACK_STATE_STARTFILE
                //


  ACE_CALLBACK_TYPE_SPACE = 4;

                // type of structure is
                // tACECallbackSpaceStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_TEMPDIRSPACE
                //                     | ACE_CALLBACK_ERROR_ARCHIVESPACE
                //


  ACE_CALLBACK_TYPE_SFXFILE = 5;

                // type of structure is
                // tACECallbackSFXFileStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // ErrorCallbackProc   | ACE_CALLBACK_ERROR_READINGSFXFILE
                //


  ACE_CALLBACK_TYPE_COPY = 6;

                // type of structure is
                // tACECallbackCopyStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // InfoCallbackProc    | ACE_CALLBACK_INFO_COPY
                //


  ACE_CALLBACK_TYPE_PROGRESS = 7;

                // type of structure is
                // tACECallbackProgressStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // StateCallbackProc   | ACE_CALLBACK_STATE_PROGRESS
                //


  ACE_CALLBACK_TYPE_CRCCHECK = 8;

                // type of structure is
                // tACECallbackCRCCheckStruc
                //-------------------------------------------------------------
                // callback function   | codes using this structure
                // --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                // StateCallbackProc   | ACE_CALLBACK_STATE_ENDCRCCHECK
                //


      //-----------------------------------------------------------------------
      // These values are passed to the ACEInfoCallbackProc callback function
      // to inform the application about actions (smaller parts of operations)
      // which may take some time or other things that might be of interest.
      //-----------------------------------------------------------------------

   ACE_CALLBACK_INFO_GENERALKEY = $100;

                                // key is a general one (no own AV; own key
                                // is obtainable for a special price!?)
                                // not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister

  ACE_CALLBACK_INFO_TMPARCCREATE = $110;

                                // creating temporary archive for changes
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACERepair


  ACE_CALLBACK_INFO_TMPARCCREATEEND = $111;

                                // finished creating temporary archive
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACERepair


  ACE_CALLBACK_INFO_ADDRECREC = $112;

                                // adding recovery record
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_INFO_ADDRECRECEND = $113;

                                // finished adding recovery record
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_INFO_RECREC = $114;

                                // trying to recover files by recovery
                                // record; end indicated by
                                // ACE_CALLBACK_INFO_RECOVERED or
                                // ACE_CALLBACK_INFO_NODAMAGE
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_INFO_NORECREC = $115;

                                // found no recovery record
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_INFO_RECOVERED = $116;

                                // archive has been fully recovered
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_INFO_NODAMAGE = $117;

                                // ACERepair detected by recovery record that
                                // the archive is not damaged
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_INFO_FNDMAINHEAD = $118;

                                // found archive header
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_INFO_FILELISTCREATE = $119;

                                // creating a file list of specified files
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_INFO_FILELISTCREATEEND = $11a;

                                // sent when creating the list of files
                                // is finished
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_INFO_FILESORT = $11b;

                                // sorting files (for solid compression)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_INFO_FILESORTEND = $11c;

                                // sorting files (for solid compression)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_INFO_COPYEND = $11d;

                                // copying a file finished
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACERepair


  ACE_CALLBACK_INFO_FILELISTADD = $140;

                                // called at creation of file list; the name
	                        // of the file just added to file list is
                                // passed in tACECallbackRealFileStruc
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_REALFILE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_INFO_COPY = $150;

                                // copying a file; file name, file size and
                                // copied bytes are passed via
                                // tACECallbackCopyStruc
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_COPY
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACERepair


  ACE_CALLBACK_ERROR_MEMORY = $200;

                                // not enough memory to perform operation
                                // (dictionary too large?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   all


  ACE_CALLBACK_ERROR_REGISTER = $201;

                                // registration key is invalid (or wrong
                                // format?); not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister


  ACE_CALLBACK_ERROR_READKEY = $202;

                                // key could not be read (does not exist or
                                // is invalid); not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACEInitDll


  ACE_CALLBACK_ERROR_WRITEKEY = $203;

                                // could not write key; not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister


  ACE_CALLBACK_ERROR_NOWINACEKEY = $204;

                                // key not valid for WinACE; not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister


  ACE_CALLBACK_ERROR_NOACTIVEACEKEY = $205;

                                // key not valid for ActiveACE; not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister


  ACE_CALLBACK_ERROR_UNCSPACE = $206;

                                // Win95_OSR1-bug: it is impossible to
                                // get available space of network drives by
                                // an UNC name; ACE will not stop but
                                // assumes there are 4Gb free
                                // --> the operation might fail if free
                                // disk space is low
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   all


  ACE_CALLBACK_ERROR_MODIFYVOLUME = $220;

                                // modification of volumes not possible
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_MODIFYLOCKEDARCHIVE = $221;

                                // modification of locked archive not possible
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_AV = $222;

                                // AV of archive is NOT ok or does not match
                                // to the users AV (not used in ACL)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEReadArchiveData
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV
                                //   ACELock
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_TOODAMAGED =  $223;

                                // can not repair by recovery record but
                                // can continue with normal repair
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_ERROR_ARCHIVEEXISTS = $224;

                                // destination file name already used;
                                // may occur if at
                                // ACE_CALLBACK_ERROR_ARCHIVESPACE a
                                // direcory is specified where a file
                                // with the same name as the current archive
                                // already exists
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_ERROR_OPENREPAIRARCHIVE = $225;

                                // could not create archive for repairing
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_ERROR_OPENARCHIVEREAD = $226;

                                // could not open archive/volume for reading
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEReadArchiveData
                                //   ACEList
                                //   ACETest
                                //   ACEExtract
                                //   ACERepair


  ACE_CALLBACK_ERROR_OPENARCHIVEWRITE = $227;

                                // could not open archive/volume for writing
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_ERROR_READARCHIVE = $228;

                                // error reading from archive
                                // (source disk removed?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEReadArchiveData
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV
                                //   ACELock
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_WRITEARCHIVE = $229;

                                // error writing to archive
                                // (destination disk removed?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_ERROR_ALREADYSFX = $22a;

                                // ca not make to SFX: is already SFX
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAddSFX


  ACE_CALLBACK_ERROR_ADDSFXTOVOLUME = $22b;

                                // adding SFX to volumes not possible
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAddSFX


  ACE_CALLBACK_ERROR_ARCHIVEBROKEN = $22c;

                                // archive is broken (damaged)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEReadArchiveData
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV
                                //   ACELock
                                //   ACEAddRecoveryRecord

  ACE_CALLBACK_ERROR_ARCHIVESAVE = $22d;

                                // not enough space to save archive;
                                // but normally
                                // ACE_CALLBACK_ERROR_ARCHIVESPACE
                                // should allow to change destination
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


   ACE_CALLBACK_ERROR_NOFILES = $22e;

                                // no files specified/could not find files
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_ERROR_ISNOTANARCHIVE = $22f;

                                // specified archive file is not an
                                // ACE archive
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEReadArchiveData
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV
                                //   ACELock
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_TEMPDIRCREATE = $230;

                                // could not create file in temp directory
                                // (write protected or directory does
                                //  not exist)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_ERROR_HIGHERVERSION = $231;

                                // this Dll version is not able to handle
                                // the archive
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_ERROR_CREATIONNAMEINUSE = $240;

                                // name used by directory
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_ENCRYPTIONCRC = $242;

                                // encryption failed because of CRC-Error at
                                // decompression
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEEncryptFiles


  ACE_CALLBACK_ERROR_READ = $243;

                                // error reading file to be added
                                // (source disk removed?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_ERROR_WRITE = $244;

                                // error at extraction
                                // (destination disk removed?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_OPENREAD = $245;

                                // error opening file for reading
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_ERROR_OPENWRITE = $246;

                                // error opening file for writing
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_FILENAMETOOLONG = $247;

                                // resulting file name too long
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_ERROR_REPACKCRC = $248;

                                // CRC-check error at recompression
                                // (archive broken or wrong password)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd


  ACE_CALLBACK_ERROR_EXCLUDEPATH = $249;

                                // could not exclude path of file names; two
                                // or more files would have the same name
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd


  ACE_CALLBACK_ERROR_METHOD = $24a;

                                // compression method not known to this
                                // Dll version
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACEEncryptFiles


  ACE_CALLBACK_ERROR_EXTRACTSPACE = $24b;

                                // not enough space to extract file
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_CREATION = $24c;

                                // creation failed (write-protection?)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_OVERWRITEDELETE = $24d;

                                // could not overwrite because deletion of
                                // file failed
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEExtract


  ACE_CALLBACK_ERROR_MOVEDELETE = $260;

                                // deletion of file or directory failed
                                // (move operation)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_REALFILE
                                // operations:
                                //   ACEAdd

  ACE_CALLBACK_ERROR_TEMPDIRSPACE = $270;

                                // not enough space at current temp directory
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_SPACE
                                // operations:
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV


  ACE_CALLBACK_ERROR_ARCHIVESPACE = $271;

                                // not enough space to save archive
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_SPACE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACELock
                                //   ACEAddAV
                                //   ACEAddRecoveryRecord
                                //   ACERepair


  ACE_CALLBACK_ERROR_READINGSFXFILE = $280;

                                // error reading SFX file:
                                // is no SFX file,
                                // file does not exist or could not be opened
                                // for reading
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_SFXFILE
                                // operations:
                                //   ACEAdd
                                //   ACEAddSFX



  ACE_CALLBACK_REQUEST_REGISTER = $300;

                                // Global.UserAV has to be set
                                // to continue registration process;
                                // not used in ACL
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_GLOBAL
                                // operations:
                                //   ACERegister


  ACE_CALLBACK_REQUEST_MARKASSOLID = $320;

                                // ArchiveHeader damaged,
                                // set solid flag for the new archive?
                                // (in case of doubt return yes!)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_REQUEST_CHANGEVOLUME = $321;
                                // Asks for permission to process next volume.
                                // If operation is ACE_CALLBACK_OPERATION_ADD
                                // then a new volume will be created.
                                // The application may change the name
                                // of the archive by modifying
                                // ArchiveData->ArchiveName
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEList
                                //   ACETest
                                //   ACEExtract


  ACE_CALLBACK_REQUEST_ARCHIVEEXISTS = $322;
                                // Asks whether to overwrite a file with
                                // the same name as the archive.
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEDelete
                                //   ACEAdd
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_REQUEST_OVERWRITE = $340;

                                // Overwrite existing file?
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd
                                //   ACEExtract


  ACE_CALLBACK_REQUEST_DELARCHIVEDSYSFILE = $341;

                                // Delete rdonly/hidden/system file
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEDelete


  ACE_CALLBACK_REQUEST_ADDBROKENFILE = $342;

                                // repair function found file with
                                // broken header, add file?
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACERepair


  ACE_CALLBACK_REQUEST_PASSWORD =  $343;

                                // password required; attention: may be
                                // decryption _and_ encryption; but  passwords
                                // can be different --> better changing
                                // passwords at StateCallbackProc
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACEEncryptFiles


  ACE_CALLBACK_REQUEST_OVERWRITESYSFILE =  $344;

                                // Overwrite rdonly/hidden/system file
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEAdd
                                //   ACEExtract


  ACE_CALLBACK_REQUEST_MOVEDELREALSYSFILE =  $360;

                                // Delete rdonly/hidden/system file
                                // (move to archive operation)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_REALFILE
                                // operations:
                                //   ACEAdd




  ACE_CALLBACK_STATE_STARTARCHIVE =  $400;

                                // procession of archive is about to begin
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles
                                //   ACEAddSFX
                                //   ACEAddAV
                                //   ACELock
                                //   ACEAddRecoveryRecord


  ACE_CALLBACK_STATE_STARTFILE =  $410;

                                // procession of file is about to begin
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_STATE_ENDNOCRCCHECK =  $411;

                                // end of file procession
                                // (no CRC chceck for this operation)
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_ARCHIVEDFILE
                                // operations:
                                //   ACEList
                                //   ACEDelete
                                //   ACEAdd
                                //   ACERepair
                                //   ACESetComments
                                //   ACEEncryptFiles


  ACE_CALLBACK_STATE_PROGRESS =  $420;

                                // informs about the progress of a file
                                // operation
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_PROGRESS
                                // operations:
                                //   ACEDelete
                                //   ACETest
                                //   ACEExtract
                                //   ACEAdd
                                //   ACERepair
                                //   ACEEncryptFiles


  ACE_CALLBACK_STATE_ENDCRCCHECK =  $430;

                                // end of file procession, CRC-check
                                // result is passed
                                //---------------------------------------------
                                // structure type:
                                //   ACE_CALLBACK_TYPE_CRCCHECK
                                // operations:
                                //   ACETest
                                //   ACEExtract
                                //   ACEDelete
                                //   ACEAdd



////////////////////////////////////////////////////////////////////////
//             PART 3.1: ACE.DLL FUNCTION RETURN CODES
////////////////////////////////////////////////////////////////////////

// These error codes are returned by the ACE.DLL-functions. The meanings
// of the codes are the same, as they are for the exit codes of ACE.EXE.

  ACE_ERROR_NOERROR   =    0;   // no error; operation succesful
  ACE_ERROR_MEM       =    1;    // insufficient memory
  ACE_ERROR_FILES     =    2;    // no files specified
  ACE_ERROR_FOUND     =    3;    // specified archive not found
  ACE_ERROR_FULL      =    4;    // disk full
  ACE_ERROR_OPEN      =    5;    // could not open file
  ACE_ERROR_READ      =    6;    // read error
  ACE_ERROR_WRITE     =    7;    // write error
  ACE_ERROR_CLINE     =    8;    // invalid command line
  ACE_ERROR_CRC       =    9;    // CRC error
  ACE_ERROR_OTHER     =   10;    // other error
  ACE_ERROR_EXISTS    =   11;    // file already exists
  ACE_ERROR_USER      =  255;    // user break (application
                                 // returned cancel code at
                                 // callback function)

// These error codes are returned by the ACE.DLL-functions. They are not
// used by ACE.EXE yet.

  ACE_ERROR_PARAM     =  128;    // might be used later


////////////////////////////////////////////////////////////////////////
//                     PART 1: DIFFERENT STRUCTURES
////////////////////////////////////////////////////////////////////////
//
// Here in different structures used at callback functions and
// ACE.DLL functions are declared.
//
// Contents:
//   Part 1.1: structures used in callback structures
//   Part 1.2: structures used in function structures
//
////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////
//             Part 1.1: structures used in callback structures
////////////////////////////////////////////////////////////////////////
// comment buffer structure
// Used in tACEGlobalDataStruc. Application has to specify where the
// comment is or should be stored.

type
  tACECommentStruc = packed record
    Buf                  : PByteArray;
    Bufsize              : integer;
    State                : integer;
  end;
  pACECommentStruc = ^tACECommentStruc;

// Global data structure
// This structure contains information for the Dll being interesting for
// nearly all functions. The Dll has to be initialized with this
// structure passed to tACEInitDll(). This structure is also passed
// by the callback functions.


  tACEGlobalDataStruc = packed record
    Obj                 : Pointer;
                                 // ---- reserved for application! ----
                                 // thought to be used as a pointer to
                                 // an object; when a callback-function is
                                 // called, the object pointer can be used to
                                 // handle the callback in a specific way;
                                 // the pointer has to be initialized by
                                 // ACEInitDll()

    MaxArchiveTestBytes : ulong; // how many bytes of a file should be
                                 // looked upon at archive header search?

    MaxFileBufSize      : ulong; // maximum buffer size for buffered
                                 // I/O operations

    Comment             : tACECommentStruc;
                                 // used to exchange comment data
                                 // between application and Dll
                                 // using callback functions

    DecryptPassword     : PChar; // the DecryptPassword specified at
                                 // ACEInitDll() is overwritten by the
                                 // DecryptPassword field of tACEAddStruc and
                                 // other function-specific structures;
                                 // but this field can be used to change the
                                 // password at callback function calls
    UseVBStructures     : LongBool; //  passes structures to callback functions
                                 //  much better suited for Visual Basic

    Reserved1           : array[0..59] of char;


// fields for ACE only
    EncryptPassword     : PChar; // things stated at the description of the
                                 // DecryptPassword field apply here as well

    TempDir             : PChar; // directory to save temporary archive

// registration (ACE DLLs only, but not used at ACL)

    KeyPath             : PChar; // necessary for reading and writing key file

    UserAV              : PChar; // Dll returns the AV string (if registered)
                                 // in this field

    IsGeneralKey        : PChar; // DLL returns the key, if it is a general key

    OwnerWindow         : HWND ; // specifies the applications window to be
                                 // parent of the registration reminder dialog

// fields for ACE only

    CompressionLevel    : ulong; // contains the currently used compression
                                 // level - may be changed during compression
                                 // operation

    Reserved2           : array[0..55] of char;
                                 // has to be filled with zeros

                                 // Callback routine addresses
    InfoCallbackProc    : Pointer;
    ErrorCallbackProc   : Pointer;
    RequestCallbackProc : Pointer;
    StateCallbackProc   : Pointer;

// different new fields
    Reserved3           : array[0..63] of char;  // has to be filled with zeros
  end;
  pACEGlobalDataStruc = ^tACEGlobalDataStruc;

////////////////////////////////////////////////////////////////////////
//  archive data structure
// Informs the callback functions about the current archive, its volume
// number, the archive-flags (see ACE_FLAG constants), the creation host
// system (see ACE_HOST constants) and the AV if present in archive.
// Also used at ACEReadArchiveData().

  tACEArchiveDataStruc = packed record
   ArchiveName   : PChar;
   VolumeNumber,
   Flags,          // see ACE_ARCFLAG defines below
   HostCreated,    // see ACE_HOST defines below
   TimeCreated,    // in MS-DOS format
   VersionCreated,
   VersionExtract: ulong; // version needed to extract files
   AV            : PChar;  // not used in ACL
   Reserved      : array[0..63] of char;  // filled with zeros
  end;
  pACEArchiveDataStruc = ^tACEArchiveDataStruc;

////////////////////////////////////////////////////////////////////////
// Contains information about an archived file.

  tACEFileDataStruc = packed record
     SourceFileName        : PChar;   // relative file name
     DestinationFileName   : PCHAR;   // absolute file name;
                                      // valid for add and extract only!
     Flags,                           // see ACE_FILEFLAG defines below
     CRC32,
     Method,                          // 0=stored, 1=LZ77, 2=V20Compression
     Dictionary                       // DictionarySize = 2^Dictionary
                            : ulong;

     CompressedSize,
     Size                   : Int64;
     Time,
     Attributes             : ulong;

     Reserved                : array[0..63] of char;
                                      // filled with zeros
  end;
  pACEFileDataStruc = ^tACEFileDataStruc;

////////////////////////////////////////////////////////////////////////
// Is passed to ACEInfoCallbackProc with ACE_CALLBACK_INFO_COPY as code.
// Informs application about the progress of copying either an archive to
// a temporary archive, or a temporary archive back to a normal archive.

  tACECopyInfoStruc = packed record
    SourceFileName         : PChar;  // source file
    DestinationFileName    : PChar;  // the file copying the source to
    CopiedBytes,                     // bytes already copied
    FileSize               : int64;  // source file size
    Reserved               : array[0..63] of char; // filled with zeros
  end;
  pACECopyInfoStruc = ^tACECopyInfoStruc;

////////////////////////////////////////////////////////////////////////
// operation progress structure
// Used to state the progress of the current operation.

  tACEProgressDataStruc = packed record
    Addr                : PChar; // address of uncompressed data block
    Size                : ulong; // size of uncompressed data block
    TotalProcessedSize  : int64; // counted by Dll:
                                 // total uncompressed bytes processed
    TotalCompressedSize : int64; // total compressed bytes processed
    TotalSize           : int64; // total uncompressed bytes to process
                                 // (sum of all original file sizes)
    FileProcessedSize   : int64; // uncompr. bytes of file processed
    FileCompressedSize  : int64; // compr. bytes of file processed
    FileSize            : int64; // uncompressed file size
  end;
  pACEProgressDataStruc = ^tACEProgressDataStruc;


////////////////////////////////////////////////////////////////////////
//             Part 1.2: structures used in function structures
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//  file list structure
// This structure is used in the function specific structures.
// The application has to use this structure to indicate which files
// have to be processed by the DLL.

  tACEFilesStruc = packed record
    SourceDir      : PChar;      // maybe a real or an archive directory
    FileList       : PChar;      // pointer to list of files to process;
                                 // zero-terminated; files have to be
                                 // separated by carriage-return (0xd);
                                 // FileList may/will be modified by the
                                 // Dll; if nothing is specified, "*"
                                 // will be used
                                 // (attention at ACEDelete!!)
     ExcludeList   : PChar;      // list of files to exclude from process
     FullMatch     : LongBool;   // specifications must fully match
                                 // (no files with the same name in
                  				           //  subdirs are processed)
     RecurseSubDirs: LongBool;   // recurse subdirectories
           			                   // (valid for add operations only)
     Reserved      : array[0..59] of char;
                                 // has to be filled with zeros
                                 // for future: possibly in-/excluding
                                 // file attributes and date/time range
  end;
  pACEFilesStruc = ^tACEFilesStruc;

////////////////////////////////////////////////////////////////////////
// V2.0 compression structure
// Specifies whether to use v2.0 compression or not. If you use v2.0
// compression you can also specify which v2.0 compression techniques
// you want to use. (ACE only)

  tACEV20CompressionStruc = packed record
    DoUse,                // if DoUse=1 and all other fields are
    DoUseDelta,           // zero, then all v2.0 compression
    DoUseExe,             // techniques will be used
    DoUsePic,
    DoUseSound  : LongBool;
    Reserved    : array[0..63] of char; // has to be filled with zeros

  end;

////////////////////////////////////////////////////////////////////////
//  compression parameter structure
// Used in tACEAddStruc and tACEDeleteStruc. (ACE only)

//        typedef struct sACECompressParamsStruc
  tACECompressParamsStruc = packed record
    Level,                      // see ACE_LEVEL constants below
    Dictionary      : ulong;    // 15(32k)..22(4Mb)

    V20Compression  : tACEV20CompressionStruc;
                                // indicates if (or which) v2.0
                                // compression techniques shall be used

    TestAfter       : LongBool; // make a test for CRC check errors
                                // after compression
    Reserved        : array[0..63] of char; // has to be filled with zeros
  end;

////////////////////////////////////////////////////////////////////////
//             PART 2: ACE.DLL CALLBACK DECLARATIONS
////////////////////////////////////////////////////////////////////////
//
// ACE.DLL makes use of four callback functions to exchange data
// with the application:
//
//   1) InfoCallbackProc   (pACEInfoCallbackProcStruc    Info)
//   2) ErrorCallbackProc  (pACEErrorCallbackProcStruc   Error)
//   3) RequestCallbackProc(pACERequestCallbackProcStruc Request)
//   4) StateCallbackProc  (pACEStateCallbackProcStruc   State)
//
// Meaning of different callback types:
//   Info    - lets the application know about actions that take some
//             time but are not essential
//             (Code is one of ACE_CALLBACK_INFO constants)
//   Error   - an error occured; if the reason for this error can
//             be solved by the application then the Dll can continue
//             the current operation, otherwise the operation has to
//             be canceled
//             (Code is one of ACE_CALLBACK_ERROR constants)
//   Request - the Dll needs some user input
//             for ex.: "Overwrite file? (yes/no/cancel)"
//             (Code is one of ACE_CALLBACK_REQUEST constants)
//   State   - Dll informs application about the progress of an operation
//             (Code is one of ACE_CALLBACK_STATE constants)
//
// The pointers to the callback functions has to be set by the application
// when calling ACEInitDll(). If the application does not install
// a callback function, is has set the corresponding pointer to NULL.
// If the ACE.DLL has to call the Error or Request callback function
// and they are not installed, the ACE.DLL will cancel the operation.
//
// The application has the possibility to cancel the current operation
// at each callback function call. So if the user clicks on a Cancel-button,
// the application should return ACE_CALLBACK_RETURN_CANCEL at the next
// callback function call.
//
// All callback function parameters are declared as unions.
// The StructureType field contains he type of the structure which is used.
// When the application knows which type of structure it has to use,
 // it will have to interpret the Code field to get to know the reason
// for the callback function call.
//
// Contents:
//   Part 2.1: operation types
//   Part 2.2: callback function return codes
//   Part 2.3: callback structure types
//   Part 2.4: callback structures
//   Part 2.5: info callback function
//   Part 2.6: error callback function
//   Part 2.7: request callback function
//   Part 2.8: state callback function




////////////////////////////////////////////////////////////////////////
//                   Part 2.4: different callback structures
// These are the declarations of the different structures used in the
// unions passed by the callback functions.

      //-----------------------------------------------------------------------
      // Only the Dll GlobalData is passed to the application.
      //-----------------------------------------------------------------------

  tACECallbackGlobalStruc = packed record
//???    StructureType : ulong;         // is ACE_CALLBACK_TYPE_GLOBAL
    Code          : ulong;         // see definition of
                                   // ACE_CALLBACK_TYPE_GLOBAL
    Operation     : ulong;         // ACE_CALLBACK_OPERATION constant

    GlobalData    : pACEGlobalDataStruc;  // see tACEGlobalDataStruc
  end;
  pACECallbackGlobalStruc = ^tACECallbackGlobalStruc;


      //-----------------------------------------------------------------------
      // The Dll GlobalData and the ArchiveData are passed.
      //-----------------------------------------------------------------------

  tACECallbackArchiveStruc = packed record
//???    StructureType   : ulong;                    // is ACE_CALLBACK_TYPE_ARCHIVE
    Code            : ulong;                    // see definition of
                                                // ACE_CALLBACK_TYPE_ARCHIVE
    Operation       : ulong;                    // ACE_CALLBACK_OPERATION constant

    GlobalData      : pACEGlobalDataStruc;      // see tACEGlobalDataStruc
    ArchiveData     : pACEArchiveDataStruc;     // see tACEArchiveDataStruc
  end;
  pACECallbackArchiveStruc = ^tACECallbackArchiveStruc;

      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData and FileData are passed.
      //-----------------------------------------------------------------------

  tACECallbackArchivedFileStruc = packed record
//???    StructureType    : ulong;                  // is ACE_CALLBACK_TYPE_ARCHIVEDFILE
    Code             : ulong;                  // see definition of
                                               // ACE_CALLBACK_TYPE_ARCHIVEDFILE
    Operation        : ulong;                  // ACE_CALLBACK_OPERATION constant

    GlobalData       : pACEGlobalDataStruc;    // see tACEGlobalDataStruc
    ArchiveData      : pACEArchiveDataStruc;   // see tACEArchiveDataStruc
    FileData         : pACEFileDataStruc;      // see tACEFileDataStruc
  end;
  pACECallbackArchivedFileStruc = ^tACECallbackArchivedFileStruc;


      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData and a FileName are passed.
      //-----------------------------------------------------------------------

  tACECallbackRealFileStruc = packed record
//???    StructureType    : ulong;                  // is ACE_CALLBACK_TYPE_REALFILE
    Code             : ulong;                  // see definition of
                                               // ACE_CALLBACK_TYPE_REALFILE
    Operation        : ulong;                  // ACE_CALLBACK_OPERATION constant

    GlobalData       : pACEGlobalDataStruc;    // see tACEGlobalDataStruc
    ArchiveData      : pACEArchiveDataStruc;   // see tACEArchiveDataStruc
    FileName         : PChar;                  // name of file
  end;
  pACECallbackRealFileStruc = ^tACECallbackRealFileStruc;

      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData, the path of temp directory and the
      // bytes required in temp directory (archive size) are passed.
      //-----------------------------------------------------------------------

  tACECallbackSpaceStruc = packed record
//???    StructureType   : ulong;                  // is ACE_CALLBACK_TYPE_SPACE
    Code            : ulong;                  // see definition of
                                              // ACE_CALLBACK_TYPE_SPACE
    Operation       : ulong;

    GlobalData      : pACEGlobalDataStruc;    // see tACEGlobalDataStruc
    ArchiveData     : pACEArchiveDataStruc;   // see tACEArchiveDataStruc
    Directory       : PChar;                  // path of directory
    ArchiveSize     : int64;                  // bytes required in temp dir
  end;
  pACECallbackSpaceStruc = ^tACECallbackSpaceStruc;

      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData and SFXFileName are passed.
      //-----------------------------------------------------------------------

  tACECallbackSFXFileStruc = packed record
//???    StructureType  : ulong;                   // is ACE_CALLBACK_TYPE_SFXFILE
    Code           : ulong;                   // see definition of
                                              // ACE_CALLBACK_TYPE_SFXFILE
    Operation      : ulong;                   // ACE_CALLBACK_OPERATION constant

    GlobalData     : pACEGlobalDataStruc;     // see tACEGlobalDataStruc
    ArchiveData    : pACEArchiveDataStruc;    // see tACEArchiveDataStruc
    SFXFileName    : PChar;                   // name of SFX
  end;
  pACECallbackSFXFileStruc = ^tACECallbackSFXFileStruc;


      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData and CopyData are passed.
      //-----------------------------------------------------------------------

  tACECallbackCopyStruc = packed record
//???    StructureType   : ulong;                 // is ACE_CALLBACK_TYPE_COPY
    Code            : ulong;                 // see definition of
                                             // ACE_CALLBACK_TYPE_COPY
    Operation       : ulong;                 // ACE_CALLBACK_OPERATION constant

    GlobalData      : pACEGlobalDataStruc;   // see tACEGlobalDataStruc
    ArchiveData     : pACEArchiveDataStruc;  // see tACEArchiveDataStruc
    CopyData        : pACECopyInfoStruc;     // see tACECopyInfoStruc
  end;
  pACECallbackCopyStruc = ^tACECallbackCopyStruc;

      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData, FileData and ProgressData are passed.
      //-----------------------------------------------------------------------

  tACECallbackProgressStruc = packed record
//???    StructureType   : ulong;                 // is ACE_CALLBACK_TYPE_COPY
    Code            : ulong;                 // see definition of
                                             // ACE_CALLBACK_TYPE_COPY
    Operation       : ulong;                 // ACE_CALLBACK_OPERATION constant

    GlobalData      : pACEGlobalDataStruc;   // see tACEGlobalDataStruc
    ArchiveData     : pACEArchiveDataStruc;  // see tACEArchiveDataStruc

    FileData        : pACEFileDataStruc;     // see tACEFileDataStruc
    ProgressData    : pACEProgressDataStruc; // see tACEProgressDataStruc
  end;
  pACECallbackProgressStruc = ^tACECallbackProgressStruc;

      //-----------------------------------------------------------------------
      // GlobalData, ArchiveData, FileData and CRC-check result are passed.
      //-----------------------------------------------------------------------

  tACECallbackCRCCheckStruc = packed record
//???    StructureType   : ulong;                 // is ACE_CALLBACK_TYPE_COPY
    Code            : ulong;                 // see definition of
                                             // ACE_CALLBACK_TYPE_COPY
    Operation       : ulong;                 // ACE_CALLBACK_OPERATION constant

    GlobalData      : pACEGlobalDataStruc;   // see tACEGlobalDataStruc
    ArchiveData     : pACEArchiveDataStruc;  // see tACEArchiveDataStruc

    FileData        : pACEFileDataStruc;     // see tACEFileDataStruc

    CRCOk           : LongBool;              // CRC-check result
  end;
  pACECallbackCRCCheckStruc = ^tACECallbackCRCCheckStruc;


////////////////////////////////////////////////////////////////////////
//                Part 2.5: info callback function
// Declaration of ACEInfoCallbackProc() parameter and explanation of
// callback info codes.

      //-----------------------------------------------------------------------
      // Union parameter used at ACEInfoCallbackProc().
      //-----------------------------------------------------------------------

  tACEInfoCallbackProcStruc = packed record
    case StructureType : ulong of
      0 : (Global    : tACECallbackGlobalStruc);
      1 : (Archive   : tACECallbackArchiveStruc);
      2 : (RealFile  : tACECallbackRealFileStruc);
      3 : (Copy      : tACECallbackCopyStruc);
  end;
  pACEInfoCallbackProcStruc = ^tACEInfoCallbackProcStruc;




  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  //=================---  Part 2.6: error callback function  ---===============
  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  // Declaration of ACEErrorCallbackProc() parameter and explanation of
  // callback error codes.
  //---------------------------------------------------------------------------

      //-----------------------------------------------------------------------
      // Union parameter used at ACEErrorCallbackProc().
      //-----------------------------------------------------------------------

  tACEErrorCallbackProcStruc = packed record
    case StructureType : ulong of
      0 : (Global        : tACECallbackGlobalStruc);
      1 : (Archive       : tACECallbackArchiveStruc);
      2 : (ArchivedFile  : tACECallbackArchivedFileStruc);
      3 : (RealFile      : tACECallbackRealFileStruc);
      4 : (Space         : tACECallbackSpaceStruc);
      5 : (SFXFile       : tACECallbackSFXFileStruc);
  end;
  pACEErrorCallbackProcStruc = ^tACEErrorCallbackProcStruc;

      //-----------------------------------------------------------------------
      // This structure is used by the ACEErrorCallback function to inform
      // the application about errors. The Code field of the used structure
      // contains an ACE_CALLBACK_ERROR value. At most problems modifications
      // to the passed structure can be made to fix it. Other problems can not
      // be solved and cause an operation abort immediately.
      // ErrorCallbackProc() has to return either ACE_CALLBACK_RETURN_OK or
      // ACE_CALLBACK_RETURN_CANCEL.
      //-----------------------------------------------------------------------


  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  //================---  Part 2.7: request callback function  ---==============
  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  // Declaration of ACERequestCallbackProc() parameter and explanation of
  // callback request codes.
  //---------------------------------------------------------------------------

      //-----------------------------------------------------------------------
      // Union parameter used at ACERequestCallbackProc().
      //-----------------------------------------------------------------------


  tACERequestCallbackProcStruc = packed record
    case StructureType : ulong of
      0 : (Global        : tACECallbackGlobalStruc);
      1 : (Archive       : tACECallbackArchiveStruc);
      2 : (ArchivedFile  : tACECallbackArchivedFileStruc);
      3 : (RealFile      : tACECallbackRealFileStruc);
  end;
  pACERequestCallbackProcStruc = ^tACERequestCallbackProcStruc;

      //-----------------------------------------------------------------------
      // Question constants are passed to the RequestCallbackProc callback
      // function to request further data.
      // RequestCallbackProc may return ACE_CALLBACK_RETURN_OK,
      // ACE_CALLBACK_RETURN_NO or ACE_CALLBACK_RETURN_CANCEL.
      //-----------------------------------------------------------------------


  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  //=================---  Part 2.8: state callback function  ---===============
  //อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  // Declaration of ACEStateCallbackProc() parameter and explanation of
  // callback state codes.
  //---------------------------------------------------------------------------

      //-----------------------------------------------------------------------
      // Union parameter used at ACEStateCallbackProc().
      //-----------------------------------------------------------------------

  tACEStateCallbackProcStruc = packed record
    case StructureType : ulong of
      0 : (Archive       : tACECallbackArchiveStruc);
      1 : (ArchivedFile  : tACECallbackArchivedFileStruc);
      2 : (RealFile      : tACECallbackRealFileStruc);
      3 : (Progress      : tACECallbackProgressStruc);
      4 : (CRCCheck      : tACECallbackCRCCheckStruc);
  end;
  pACEStateCallbackProcStruc = ^tACEStateCallbackProcStruc;

      //-----------------------------------------------------------------------
      // Calls to (*StateCallbackProc)() with ACE_CALLBACK_STATE values in the
      // Code field are made to enable the application to show the progress of
      // an operation.
      //-----------------------------------------------------------------------


////////////////////////////////////////////////////////////////////////
//             Part 3.2: functions and parameter structures
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// ACEInitDll
// Initializes ACE dynamic link library. Has to be called before any
// other function call. May be called more than one time.

// ACEInitDll() parameter structure.

  tACEInitDllStruc = packed record
    GlobalData : tACEGlobalDataStruc;
    Reserved   : array[0..63] of char; // has to be filled with zeroes
  end;
  pACEInitDllStruc = ^tACEInitDllStruc;

// ACEInitDll() function declaration.
  TACEInitDllProc = function(DllDate : pACEInitDllStruc) : integer; stdcall;


////////////////////////////////////////////////////////////////////////
// ACEReadArchiveData
// Tests a file whether it is an archive or not and reads out the archive
// data.

// ACEReadArchiveData() parameter structure.

  tACEReadArchiveDataStruc = packed record
    ArchiveData : tACEArchiveDataStruc;   // if this pointer is NULL, the
                                          // file passed to ACEReadArchiveData
                                          // is no archive; otherwise it points
                                          // to a tACEArchiveDataStruc structure
                                          // that contains information about the
                                          // archive
    Reserved    : array[0..63] of char;   // has to be filled with zeroes
  end;
  pACEReadArchiveDataStruc = ^tACEReadArchiveDataStruc;


// ACEReadArchiveData() function declaration.
  TACEReadArchiveDataProc = function(ArchiveName : PChar;
                                     ArchiveData : pACEReadArchiveDataStruc) : integer; stdcall;

////////////////////////////////////////////////////////////////////////
//  ACEList
// Passes the specified files in the archive to StateCallbackProc().

// ACEList() parameter structure.

  tACEListStruc = packed record
    Files : tACEFilesStruc;           // specifies files to be listed;
                                      // see tACEFilesStruc structure
    Reserved : array[0..63] of char;  // has to be filled with zeroes
  end;
  pACEListStruc = ^tACEListStruc;

// ACEList() function declaration.
  TACEListProc = function(ArchiveName : PChar;
                          List        : pACEListStruc) : integer; stdcall;


////////////////////////////////////////////////////////////////////////
// ACETest
// Tests specified files in archive.

// ACETest() parameter structure.
  tACETestStruc = packed record
    Files : tACEFilesStruc;           // specifies files to test;
                                      // see tACEFilesStruc structure
    DecryptPassword : PChar;          // zero-terminated string,
                                      // case-sensitive (maxlen=56)
    Reserved : array[0..63] of char;  // has to be filled with zeroes
  end;
  pACETestStruc = ^tACETestStruc;

// ACETest() function declaration.
  TACETestProc = function(ArchiveName : PChar;
                          List        : pACETestStruc) : integer; stdcall;

////////////////////////////////////////////////////////////////////////
// ACEExtract
// Extracts specified  files.

// ACEExtract() parameter structure.
  tACEExtractStruc = packed record
    Files : tACEFilesStruc;           // specifies files to extract;
                                      // see tACEFilesStruc structure
    DestinationDir  : PChar;          // directory to extract files to
    ExcludePath     : LongBool;       // extract files without path
    DecryptPassword : PChar;          // password for decryption (if files
                                      // are encrypted);
                                      // zero-terminated string,
                                      // case-sensitive (maxlen=56)
    Reserved : array[0..63] of char;  // has to be filled with zeroes
  end;
  pACEExtractStruc = ^tACEExtractStruc;

// ACEExtract() function declaration.
  TACEExtractProc = function(ArchiveName : PChar;
                             Extract     : pACEExtractStruc) : integer; stdcall;


const
  FILELISTSIZE   = 32768;   // pretty much for this this example:
                            // only the commandline can be used to
                            // specify files..
  COMMENTBUFSIZE =  8192;   // comments may be up to 32k in size
                            // increase it if you want to put that
                            // large comments to archives, or if
                            // you want to receive all of these large
                            // comments (ACE_COMMENT_SMALLBUF returned
                            // if comment does not fit into buffer)


var
  ACEInitDll         : TACEInitDllProc;
  ACEReadArchiveData : TACEReadArchiveDataProc;
  ACEList            : TACEListProc;
  ACETest            : TACETestProc;
  ACEExtract         : TACEExtractProc;


//==========================================================================

function UnAceDllLoaded(): boolean;
function ExtractAceFile(szArchiveFileName: PChar;
                        szFileToExtract  : PChar;
                        szTargetFileName : PChar;
                        iMaxSizeToExtract: longint;
                        iArchiveOffset   : longint): longint;


implementation

uses
  {$ifdef mswindows}
  Windows
  {$ELSE}
  {$ENDIF}
  UBaseUtils;

var
  //FileList           : array[0..FILELISTSIZE-1] of char;
  //CommentBuf         : array[0..COMMENTBUFSIZE-1] of char;

  DllHandle: THandle;
  DllName  : String;
  ZString  : array[0..256] of char;
  iAceDllLoaded: integer;
  //DllData  : tACEInitDllStruc;
  //zTempDir : array[0..255] of char;

//-----------------------------------------------------------------------------

function UnAceDllLoaded(): boolean;
  begin
  Result := false;
  {$ifdef mswindows}
  if (iAceDllLoaded = -1) then // we did not try it yet
    begin
    iAceDllLoaded := 0;
    DllName := 'UnAceV2.Dll';
    DllHandle := LoadLibrary(StrPCopy(ZString, GetProgramDir + DllName));
    if DllHandle <= 32 then exit;

    @ACEInitDll         :=GetProcAddress(DllHandle,'ACEInitDll');
    @ACEReadArchiveData :=GetProcAddress(DllHandle,'ACEReadArchiveData');
    @ACEList            :=GetProcAddress(DllHandle,'ACEList');
    @ACETest            :=GetProcAddress(DllHandle,'ACETest');
    @ACEExtract         :=GetProcAddress(DllHandle,'ACEExtract');
    if (@ACEInitDll=nil) or
       (@ACEReadArchiveData=nil) or
       (@ACEList=nil) or
       (@ACETest=nil) or
       (@ACEExtract=nil) then
      begin
      FreeLibrary(DllHandle);
      exit;
      end;
    iAceDllLoaded := 1;
    end;
  Result := iAceDllLoaded = 1;
  {$else}

  {$endif}
  end;

//-----------------------------------------------------------------------------

var
  AceCommentBuf: array[0..COMMENTBUFSIZE-1] of char;


// ACE callback functions
function UnAceInfoProc(Info : pACEInfoCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_OK;
  end;

function UnAceErrorProc(Error : pACEErrorCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_CANCEL;
  end;

function UnAceRequestProc(Request : pACERequestCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_CANCEL;
  end;

function UnAceStateProc(State: pACEStateCallbackProcStruc) : integer; stdcall;

  begin
  Result:=ACE_CALLBACK_RETURN_OK;
  end;

//-----------------------------------------------------------------------------

function ExtractAceFile(szArchiveFileName: PChar;
                        szFileToExtract  : PChar;
                        szTargetFileName : PChar;
                        iMaxSizeToExtract: longint;
                        iArchiveOffset   : longint): longint;

  var
    DllData          : tACEInitDllStruc;
    //List             : tACEListStruc;
    zTempDir         : array[0..255] of char;
    Extract          : tACEExtractStruc;
    zDestinationDir  : array[0..255] of char;
    sDestinationDir  : ShortString;
    zFileName        : array[0..255] of char;

  begin
  Result := 100; // dummy error value
  if not UnAceDllLoaded then exit;
  FillChar(DllData, SizeOf(DllData), 0);
  DllData.GlobalData.MaxArchiveTestBytes := $1ffFF;      // search for archive
                                                         // header in first 128k
                                                         // of file
  DllData.GlobalData.MaxFileBufSize      := $2ffFF;      // read/write buffer size
                                                         // is 256k
  DllData.GlobalData.Comment.BufSize     := SizeOf(AceCommentBuf)-1;
  DllData.GlobalData.Comment.Buf         := @AceCommentBuf; // set comment bufffer
                                                         // to receive comments
                                                         // of archive and/or
  {$ifdef mswidnows}                                                   // set comments
  GetTempPath(255, @zTempDir);
  DllData.GlobalData.TempDir             := @zTempDir;
  {$else}
  DllData.GlobalData.TempDir := PChar(GetTempDir);
  {$endif}
  // set callback function pointers
  DllData.GlobalData.InfoCallbackProc    := @UnAceInfoProc;
  DllData.GlobalData.ErrorCallbackProc   := @UnAceErrorProc;
  DllData.GlobalData.RequestCallbackProc := @UnAceRequestProc;
  DllData.GlobalData.StateCallbackProc   := @UnAceStateProc;
  if (ACEInitDll(@DllData) <> 0) then exit;

  FillChar(Extract, SizeOf(Extract), 0);           // set all fields to zero
  Extract.Files.SourceDir      := '';              // archive main directory is
                                                   // base dir for FileList
  Extract.Files.FileList       := szFileToExtract; // set FileList
  Extract.Files.ExcludeList    := '';              // no files to exclude
  Extract.Files.FullMatch      := true;            // also extract files
                                                   // partially matching
  sDestinationDir := ExtractFileDir(szTargetFileName);
  StrPCopy(zFileName, ExtractFileName(szFileToExtract));

  Extract.DestinationDir       := StrPCopy(zDestinationDir, sDestinationDir);
                                         // directory to extract to
  Extract.ExcludePath          := true;  // extract files with path - NO
  Extract.DecryptPassword      := '';
  Result := ACEExtract(szArchiveFileName, @Extract);
  strcat(@zDestinationDir, '\'#0);
  strcat(@zDestinationDir, zFileName);
  ///MoveFile(@zDestinationDir, szTargetFileName);
  end;

//-----------------------------------------------------------------------------

begin
iAceDllLoaded := -1;
end.
