unit ULang;
(*====================================================================
All language dependent strings, except for the strings in the forms 
======================================================================*)

interface

const

{$ifdef CZECH}

{QB_CONV4}
lsInconsistencyOfDataInDBase = 'Nekonzistence dat v databázi';

{QDBASE}
lsDBaseCannotBeOpened = 'Databázi nelze otevřít';
lsError = 'Chyba';
lsName = 'Jméno';
lsDateAndTime = 'Datum a čas';
lsSize = 'Velikost';
lsFolder = '(složka)';
lsDisk3 = '(disk)';
lsDescription = 'Popiska';
lsFolder1 = '(složka)  ';
lsSeriousProblemWritingToDBase =
   'Vyskytl se závažný problém při zápisu do databáze.'#13#10'Doporučuji vám program ihned ukončit.';
lsErrorInDBaseFound = 'Byla nalezena chyba ve struktuře databáze. '#13#10'Doporučuji vám databázi uzavřít a opravit.';
lsDrive = 'Jednotka ';
lsIsNotAccesible = ' není dostupná.';
lsLabelDifferent_Change = 'Zadané jméno je odlišné od skutečného jména disku. Zmìnit též pojmenování disku?';
lsDifferentNames = 'Rozdílná jména';
lsOnDisk = 'Na disku ';
lsLimitedLength = ' může mít pojmenování délku pouze 12 znakù.';
lsTotalFolders = ' ';
lsTotalFiles   = ' složek, ';
lsTotalSize    = ' souborù, ';

lsVolumeLabel = 'Pojmenování disku ';
lsCannotBeChanged = ' nelze změnit.';
lsOverwriteExistingRecord = 'Přepsat existující disk ';
lsWarning = 'Varování';

lsRecordCannotBeDeleted = 'Nelze smazat disk ';
lsProblem = 'Problém';

lsImportConversion =
 'Při importu je prováděna konverze kódování znaků. Chcete provést '+
 'konverzi z kódu Kamenických? (Odpověď "Ne" znamená konverzi z kódování Latin2.)';
lsImportFrom4 = 'Import databáze z formátu QuickDir verze 4.1';
lsRecordsSelected = 'Vybráno diskù: ';
lsDeleteAll = '. Smazat všechny?';
lsConfirmation = 'Potvrzení';
lsDeleteRecord = 'Smazat disk "';
lsNewNameForUndeletedDisk = 'Nové jméno pro obnovený disk';
lsDiskWithName = 'Disk se jménem "';
lsAlreadyExists = '" již existuje.';
lsDiskNameChange = 'Změna jména disku';
lsNameMustBeUnique = 'Jméno musí být unikátní';
lsNoSelectedDisk   = 'Je požadováno hledání ve vybraných discích, ale žádný není vybrán.';
lsAllDisksSelected = 'Je požadováno hledání v nevybraných discích, ale všechny jsou vybrány.';
lsTooManyFilesSelected = 'Je vybráno příliž mnoho souborù. Budou vyhledány jen některé.';
lsNotification = 'Upozornění';

lsDescriptionCannotBeByParentDir = 'Odkaz na nadøazenou složku nemůže mít popisku.';

lsListOfFreeSpace = ': Výpis volného místa';
lsDescriptionCannotBeSaved = 'Popisku nelze uložit.';
lsSelectionOfDisks = 'Výběr diskù';
lsEnterMaskForSelectionOfDisks = 'Zadejte masku pro výběr disků:';
lsSelectionOfFiles = 'Výběr souborů';
lsEnterMaskForSelectionOfFiles = 'Zadejte masku pro výběr souborů:';
lsSelectionTooLargeForClipboard = 'Výběr je příliž velký, nelze okopírovat do schránky.';
lsCannotCopy = 'Nelze kopírovat';
lsDisk1 = 'Disk: ';
lsFolder2 = 'Složka: ';
lsPage = 'Strana ';
lsQDir = 'DiskBase ';
lsPreparingToPrint = 'Připravuje se tisk...';
lsQuickDir = 'DiskBase';
lsPrintingPage = 'Tiskne se strana ';
lsSkippingPage = 'Přeskakuje se strana ';
lsDatabase = 'Databáze: ';

lsVolumesDifferent1 =
  'Jméno disku v databázi (%s) a jméno disku, jehož obsah bude čten (%s), se liší. ';
lsVolumesDifferent2 =
  'To může znamenat, že se chystáte sejmout obsah jiného disku, než je ten, jehož ';
lsVolumesDifferent3 =
  'obraz má být v databázi aktualizován. Mám přesto pokračovat?';

lsFileCannotBeOpen    = #13#10#13#10'Soubor není možné otevřít. Možné příčiny:'#13#10;
lsFolderCannotBeOpen    = #13#10#13#10'Složku není možné otevřít. Možné příčiny:'#13#10;
lsDiskNotAccessible   = ' - disk není dostupný'#13#10;
lsDifferentDisk       = ' - v mechanice je jiný disk'#13#10;
lsFileInArchive       = ' - soubor se nachází v archivu (ZIP, ARJ, atd.)'#13#10;
lsFolderInArchive     = ' - složka se nachází v archivu (ZIP, ARJ, atd.)'#13#10;
lsDiskIsNetworkDrive  = ' - disk je síťový a není právě dostupný'#13#10;
lsVolumeLabelDiffers  = ' - liší se jména disků: jde pravděpodobně o jiný disk'#13#10;
lsFileProbablyDeleted = ' - soubor byl smazán, přejmenován nebo přemístěn'#13#10;
lsFolderProbablyDeleted = ' - složka byla smazána, přejmenována nebo přemístěna'#13#10;
lsFileNotFound        = 'Soubor nenalezen';
lsFolderNotFound      = 'Složka nenalezena';

{QDBINFO}

lsYes = 'ano';
lsNo  = 'ne';

{QDESC}

lsDescrTextModified = 'Text popisky byl modifikován. Uložit?';

{QDPRINTS}

lsPrintSelection  = 'Výběr tisku';
lsPrintWhat       = 'Tisknout:';
lsExportSelection = 'Výběr položek k exportu';
lsExportWhat      = 'Exportovat:';

{QDEXPORT}
lsDefaultFilter       = 'Všechny soubory (*.*)|*.*';
lsNotValidFormatFile  = 'Tento soubor neobsahuje platný formát pro export z DiskBase.';
lsPreparingToExport   = 'Příprava na export...';
lsProcessed           = 'Zpracováno ';
lsFiles_Written       = ' souborů, zapsáno ';
lsExportFolder        = 'Export';

{QDPRINT}

lsCalculatingPrintExtent = 'Zjišťuje se rozsah tisku...';
lsDisk = 'Disk: ';
lsDateTime = 'Datum a čas';
lsExpectedNoOfPages = 'Předpokládaný počet stran je ';
lsReallyPrintThem = '. Chcete skutečně všechny vytisknout?';
lsPreparingPrint = 'Připravuje se tisk...';

{QE_Archi}

lsExploringContent = 'Zjišťuji obsah ';
lsCentralDirCPBNotFound = '   Centrální adresář CP-Backup nenalezen  ';

{QE_Main}
lsError1000 = '(chyba 1000)';
lsError1001 = '(chyba 1001)';
lsError1002 = '(chyba 1002)';
lsError1003 = '(chyba 1003)';
lsError1004 = '(chyba 1004)';
lsError1005 = '(chyba 1005)';
lsInconsitencyOfDataInDBase = 'Nekonzistence dat v databázi ';
lsNotEnoughSpaceOnDisk = 'Není dostatek místa na disku s databází.';
lsLoadingLibrary = 'Zavádím knihovnu ';
lsExtracting = 'Dekomprimuji ';
lsFrom = ' z ';
lsLibrary = 'Knihovna ';
lsDoesNotContainProperFunctions = ' neobsahuje všechny potřebné funkce.';
lsLibrary1 = 'Knihovnu ';
lsCannotBeOpened = ' nelze otevřít.';
lsReleasingLibrary = 'Uvolňuji knihovnu ';
lsNotAQDirDBase = 'Tento soubor není databáze DiskBase.';
lsRequiresNewerVersion = 'Tato databáze vyžaduje novější verzi programu.';
lsCorruptedHeader = 'Databáze má poškozené záhlaví.';
lsTrialPeriodExpired =
  'Lhůta pro použití databáze neregistrovanou verzí vypršela. Prosím registrujte se.';
lsTrialOpenCountExpired =
  'Počet použití databáze neregistrovanou verzí je vyčerpán. Prosím registrujte se.';
lsErrorReading = 'Chyba při čtení: ';
lsErrorWriting = 'Chyba při zápisu: ';
lsDBaseIsFull = 'Databáze je plná. Proveďte komprimaci.';
lsDiskInfo = 'DiskInfo.txt';
lsDiskInfoName = 'DiskInfo';
lsDiskInfoExt  = '.txt';

lsDiskInfoUpperCase = 'DISKINFO.TXT';
lsDirInfo = 'DirInfo';

lsDefaultMasks = '*.txt|500||*.doc|1000|Uni|*.wri|500|Uni|*.htm|500|HTML|*.html|500|HTML|*.602|500|T602|*.rtf|500|RTF|*.mp3|1000|MP3Tag|';

{QExcept}

lsCriticalError = 'Kritická chyba';
lsCriticalError1 = 'Kritická chyba.';
lsDBaseStructError = 'Nalezena chyba ve struktuře databáze. Zavřete databázi a vyvolejte Opravu (v menu Soubor). ';


{QFindE}
lsNoDiskFound = 'Nebyl nalezen žádný disk.';
lsInformation = 'Informace';

{QFindF}
lsNoFileFound = 'Nebyl nalezen žádný soubor.';

{QFindFD}
lsNoMaskEntered = 'Nebyl zadán žádný text k vyhledání.';
lsCannotSearch   = 'Nelze vyhledávat';

{QFoundE, QFoundF}
lsSorting = 'Řazení';
lsDisk2 = 'Disk';
lsFreeSpace = 'Volno';
lsFreeSpacePerCent = 'Volno %';
lsFreeSpacePerCent1 = ' Volno % ';
lsSelectionTooLarge = 'Výběr je příliš velký, nelze okopírovat do schránky.';
lsFolder3 = 'Složka';
lsFile = 'Soubor';

{QI_Face}
lsErrorDBaseIsNil         = 'Interní chyba 500: DBaseIsNil';
lsErrorTreeStructIsNil    = 'Interní chyba 501: TreeStructIsNil';
lsErrorCurDirColIsNil     = 'Interní chyba 502: CurDirColIsNil';
lsErrorFileSearchColIsNil = 'Interní chyba 503: FileSearchColIsNil';
lsErrorKeyFieldIsNil      = 'Interní chyba 503: KeyFieldIsIsNil';
lsErrorCurFileColIsNil    = 'Interní chyba 504: CurFileColIsNil';
lsErrorRootDirColIsNil    = 'Interní chyba 505: RootDirColIsNil';
lsErrorDirColHandleIsNil  = 'Interní chyba 506: DirColHandleIsNil';
lsErrorFileColHandleIsNil = 'Interní chyba 507: FileColHandleIsNil';

{QLocOpt}
lsMask = 'Maska';
lskB = 'Bajtů';
lsDLL = 'Filtr';

{QMain}
lsFatalInternalError = ' (Fatální interní chyba ';
lsDbaseWithThisNameIsOpen = 'Databáze s tímto jménem je otevřena - soubor nelze přepsat.';
lsCannotCreate = 'Nelze založit';
lsDBaseCannotBeCreated = 'Databázi nelze vytvořit';
lsFileDoesNotExist = 'Soubor neexistuje: ';
lsErrorOnCommandLine = 'Chyba na příkazovém řádku';
lsErrorInProgramSettings = 'Chyba v nastavení programu';
lsInvalidParameter = 'Neplatný parametr: ';
lsOptionsCannotBeMixed = 'Volby /f a /s nelze na příkazovém řádku uvést zároveň.';
lsInvalidOption = 'Neplatná volba: ';
lsDBaseIsOpenCannotCompress =
  'Databáze s tímto jménem je otevřena - pro kompresi je nutno ji uzavřít.';
lsCannotCompress = 'Nelze komprimovat';
lsCannotCompressAsItIsProtected =
  'Databázi s tímto jménem nelze komprimovat, neboť se jedná o chráněnou verzi.';
lsDbaseIsOpenCannotOverwrite = 'Databáze s tímto jménem je otevřena - nelze ji přepsat.';
lsCannotExport = 'Nelze exportovat';
lsDBaseIsOpenMustBeClosedFirst =
  'Databáze s tímto jménem je otevřena. Aby bylo možné ji importovat, je nutno jí zavřít.';
lsCannotImport = 'Nelze importovat';
lsCannotImportAsItIsProtected =
  'Databázi s tímto jménem nelze importovat, neboť se jedná o chráněnou verzi.';
lsCannotImportProbablyCorrupted =
  'Tuto databázi nelze importovat. Buď je poškozena, nebo vyžaduje novější verzi programu.';
lsAttentionMakeReindexFirst =
  'Upozornění: Před importem je potřeba provést reindexaci databáze, kterou chcete importovat.';

lsCannotContinue = 'Nelze pokračovat';
lsDBaseIsOpenCannotRepair =
  'Databáze s tímto jménem je otevřena - pro opravu je nutno ji uzavřít.';
lsCannotRepairCorruptedHeader =
  'Tento soubor buď není databází DiskBase nebo má neopravitelně poškozenou hlavičku.';
lsCannotRepairByOldVersion =
  'Tato databáze byla vytvořena novější verzí programu DiskBase; touto verzí programu ji nelze opravovat.';
lsCannotRepairRuntimeDatabase =
  'Tuto databázi nelze opravovat, neboť se jedná o chráněnou verzi.';
lsDatabaseRepair = 'Oprava databáze';
lsRepairingIndex = 'Probíhá reindexace tabulky...';

lsScannningDatabase = 'Zjišťuji obsah databáze, prosím vyčkejte...';
lsEject = 'Vysunout disk z ';

{QReindex}
lsCompressingDatabase = 'Probíhá komprese databáze';
lsExportingToRunTime = 'Probíhá export jen pro čtení';
lsExporting = 'Probíhá export';
lsImporting = 'Probíhá import';

lsCompressNotSuccessful = 'Komprese neúspěšná';
lsExportNotSuccessful = 'Export neúspěšný';
lsImportNotSuccessful = 'Import neúspěšný';
lsSourceDBase = 'Zdrojovou databázi ';
lsTargetDBase = 'Cílovou databázi ';
lsCannotBeCreated = ' nelze založit.';
lsProcessNotSuccessful = 'Proces nebyl úspěšný';

lsDBase = 'Databázi ';
lsCannotBeRenamedTo = ' nelze přejmenovat na ';
lsOriginalDBase = 'Původní databázi ';
lsCannotBeDeleted = ' nelze smazat.';

{QScanDrv}
lsIsItLastCPBDisk = 'Jedná se o disk s posledním CPB archivem?';
lsCPBFound = 'Nalezen soubor typu CPB';
lsScanArchive = 'Zjistit obsah archivu ';
lsScanningTree = 'Zjišťuje se stromová struktura:';
lsScanningFoldersAndFiles = 'Zjišťuje se obsah složek a archivů:';

{QSetting}
lsBold = ', tučné';
lsItalics = ', kurzíva';
lsArial = 'Arial CE';
lsMSSansSerif = 'MS Sans Serif';
lsScript = 'CentralEurope';
lsCompressionNecessary =
  'POZOR: Pokud změníte toto nastavení, je potřeba provést kompresi všech databází. Viz Nápověda.';

{QStream}
lsFileAccessError = 'Chyba pøi přístupu k souboru.';
lsFileCreationError = 'Chyba při vytvoření nebo otevření souboru.';
lsFileReadingError = 'Chyba při čtení souboru.';
lsFileWritingError = 'Chyba při zápisu do souboru.';
lsFileSeekingError = 'Chyba při vyhledávání v souboru.';
lsFileError = 'Chyba souboru: ';

{QRegNote}
lsOk = 'OK';
lsEndProgram = 'Konec';
lsRegisterNow = 'Registrovat';

lsNonameFile = 'Bezjmena.qdr';
lsExportFile = 'Export.qdr';

{QScanDir}
lsInvalidFolder = 'Neplatná složka. Zadejte úplnou cestu, včetně disku.';

lsScanFolderAsDisk = 'Přečíst složku jako disk';
lsScanFolder       = 'Přečíst složku:';
lsAndSaveItAsDisk  = 'a uložit do databáze jako disk:';

lsRescanDisk       = 'Aktualizovat disk';
lsRescanFolder     = 'Znovu přečíst obsah disku (složky):';
lsAndSaveItAsUpdateOfDisk = 'a uložit do databáze jako aktualizaci disku:';
lsRescanWarning    =
'Specifikace disku/složky je odlišná od původní. To může způsobit ztrátu popisek. Pokračovat?';

{QCollect}
lsCollectionOverflow = 'Příliž velký seznam nebo nedostatek paměti';
lsInvalidIndex       = 'Interní problém při indexaci seznamu';
lsAbstractMethodUsed = 'Interní problém (abstraktní metoda)';


{QFAskLab}
lsGenerated          = 'Vytvořené jméno: ';

{QFExport}
lsExportOnlySelected = 'Exportovat pouze vybrané položky?'#13#10'Ne = exportovat všechny.';
lsMultipleItemsSelected = 'Je vybrána skupina položek.';
lsFilterDatabase  = 'Export formát pro databázi (*.defd)|*.defd|Všechny soubory (*.*)|*.*';
lsFilterFileList  = 'Export formát pro soubory (*.deff)|*.deff|Všechny soubory (*.*)|*.*';
lsFilterEmptyList = 'Export formát pro volná místa (*.defs)|*.defs|Všechny soubory (*.*)|*.*';

{$endif}

{====================================================================}

{$ifdef ENGLISH}

{QB_CONV4}
lsInconsistencyOfDataInDBase = 'Inconsistency of data in the database.';

{QDBASE}
lsDBaseCannotBeOpened = 'The database cannot be open';
lsError = 'Error';
lsName = 'Name';
lsDateAndTime = 'Date and time';
lsSize = 'Size';
lsFolder = '(folder)';
lsDisk3 = '(disk)';
lsDescription = 'Description';
lsFolder1 = '(folder)  ';
lsSeriousProblemWritingToDBase =
   'Serious problem occurred when writing to database.'#13#10'Immediate program shutdown is recommended.';
lsErrorInDBaseFound = 'Database inconsistency was found.'#13#10'Close the database and repair it.';
lsDrive = 'Drive ';
lsIsNotAccesible = ' is not accessible.';
lsLabelDifferent_Change = 'Entered name is different from the current Volume Label. Change the Volume Label?';
lsDifferentNames = 'Different names';
lsOnDisk = 'The Volume Label can have max. 12 characters on disk ';
lsLimitedLength = '';
lsTotalFolders = ' ';
lsTotalFiles   = ' folders, ';
lsTotalSize    = ' files, ';

lsVolumeLabel = 'Volume Label ';
lsCannotBeChanged = ' cannot be changed.';
lsOverwriteExistingRecord = 'Overwrite existing disk ';
lsWarning = 'Warning';

lsRecordCannotBeDeleted = 'The disk cannot be deleted.';
lsProblem = 'Problem';

lsRecordsSelected = 'Disks selected: ';
lsDeleteAll = '. Delete them all?';
lsConfirmation = 'Confirmation';
lsDeleteRecord = 'Delete disk "';
lsNewNameForUndeletedDisk = 'New name for undeleted disk';
lsDiskWithName = 'Disk with name "';
lsAlreadyExists = '" already exists.';
lsDiskNameChange = 'Disk name change';
lsNameMustBeUnique = 'The name must be unique';
lsNoSelectedDisk   = 'Search in selected disks is required, but no disk is selected.';
lsAllDisksSelected = 'Search in not selected disks is required, but all disks are selected.';
lsTooManyFilesSelected = 'Too many files selected. Not all of them will be searched.';
lsNotification = 'Notification';

lsDescriptionCannotBeByParentDir = 'Reference to the parent folder cannot have a description.';

lsListOfFreeSpace = ': List of free space';
lsDescriptionCannotBeSaved = 'The description cannot be saved.';
lsSelectionOfDisks = 'Selection of disks';
lsEnterMaskForSelectionOfDisks = 'Mask for selection of disks:';
lsSelectionOfFiles = 'Selection of files';
lsEnterMaskForSelectionOfFiles = 'Mask for selection of files:';
lsSelectionTooLargeForClipboard = 'Selection too large; cannot copy to Clipboard.';
lsCannotCopy = 'Cannot copy';
lsDisk1 = 'Disk: ';
lsFolder2 = 'Folder: ';
lsPage = 'Page ';
lsQDir = 'DiskBase ';
lsPreparingToPrint = 'Preparing to print...';
lsQuickDir = 'DiskBase';
lsPrintingPage = 'Printing page ';
lsSkippingPage = 'Skipping page ';
lsDatabase = 'Database: ';
lsVolumesDifferent1 =
  'The original volume label (%s) and actual volume label (%s) are different. ';
lsVolumesDifferent2 =
  'This may indicate, that you are not scanning the same disk, that is recorded in the database. ';
lsVolumesDifferent3 =
  'Are you still sure you want to rescan the disk?';

lsFileCannotBeOpen    = #13#10#13#10'File cannot be open. Possible causes:'#13#10;
lsFolderCannotBeOpen    = #13#10#13#10'Folder cannot be open. Possible causes:'#13#10;
lsDiskNotAccessible   = ' - disk is not accessible'#13#10;
lsDifferentDisk       = ' - another disk in the drive'#13#10;
lsFileInArchive       = ' - file is in compressed archive'#13#10;
lsFolderInArchive     = ' - folder is in compressed archive'#13#10;
lsDiskIsNetworkDrive  = ' - disk is a network drive, not accessible now'#13#10;
lsVolumeLabelDiffers  = ' - volume labels differ: probably different disk'#13#10;
lsFileProbablyDeleted = ' - file was deleted, moved or renamed'#13#10;
lsFolderProbablyDeleted = ' - folder was deleted, moved or renamed'#13#10;
lsFileNotFound          = 'File Not Found';
lsFolderNotFound        = 'Folder Not Found';

{QDBINFO}

lsYes = 'yes';
lsNo  = 'no';

{QDESC}

lsDescrTextModified = 'Description text was modified. Save?';

{QDPRINTS}

lsPrintSelection = 'Print Selection';
lsPrintWhat      = 'Print:';
lsExportSelection = 'Items To Be Exported';
lsExportWhat      = 'Export:';

{QDEXPORT}
lsDefaultFilter       = 'All files (*.*)|*.*';
lsNotValidFormatFile  = 'This file does not contain valid data for DiskBase export.';
lsPreparingToExport   = 'Preparing to export...';
lsProcessed           = 'Processed ';
lsFiles_Written       = ' files, written ';
lsExportFolder        = 'Export';
lsExportDBaseFormatsFolder = lsExportFolder + '\Formats for database export';
lsExportFoundFormatsFolder = lsExportFolder + '\Formats for list export';

{QDPRINT}

lsCalculatingPrintExtent = 'Calculating print extent...';
lsDisk = 'Disk: ';
lsDateTime = 'Date and time';
lsExpectedNoOfPages = 'Expected number of pages is ';
lsReallyPrintThem = '. Are you sure you want to print them all?';
lsPreparingPrint = lsPreparingToPrint;

{QE_Archi}

lsExploringContent = 'Scanning contents of ';
lsCentralDirCPBNotFound = '   Central dir of PC-Backup not found  ';

{QE_Main}
lsError1000 = '(error 1000)';
lsError1001 = '(error 1001)';
lsError1002 = '(error 1002)';
lsError1003 = '(error 1003)';
lsError1004 = '(error 1004)';
lsError1005 = '(error 1005)';
lsInconsitencyOfDataInDBase = 'Inconsisteny of data in database ';
lsNotEnoughSpaceOnDisk = 'Insufficient free space on disk with database ';
lsLoadingLibrary = 'Loading library ';
lsExtracting = 'Extracting ';
lsFrom = ' from ';
lsLibrary = 'Library ';
lsDoesNotContainProperFunctions = ' does not contain all needed functions.';
lsLibrary1 = 'The library ';
lsCannotBeOpened = ' cannot be open.';
lsReleasingLibrary = 'Releasing library ';
lsNotAQDirDBase = 'This file is not a DiskBase database.';
lsRequiresNewerVersion = 'This database requires newer version of the program.';
lsCorruptedHeader = 'The database has corrupted header.';
lsErrorReading = 'Error reading: ';
lsErrorWriting = 'Error writing: ';
lsDBaseIsFull = 'Database is full. Make compression.';
lsDiskInfo = 'DiskInfo.txt';
lsDiskInfoName = 'DiskInfo';
lsDiskInfoExt  = '.txt';
lsDiskInfoUpperCase = 'DISKINFO.TXT';
lsDirInfo = 'DirInfo';

lsDefaultMasks = '*.txt|500||*.doc|1000|Uni|*.wri|500|Uni|*.htm|500|HTML|*.html|500|HTML|*.rtf|500|RTF|*.mp3|1000|MP3Tag|';

{QExcept}
lsCriticalError = 'Critical error';
lsCriticalError1 = 'Critical error.';
lsDBaseStructError = 'Error in database structure found. Close the database and call Repair from the File menu. ';

{QFindE}
lsNoDiskFound = 'No disk found.';
lsInformation = 'Information';

{QFindF}
lsNoFileFound = 'No file found.';

{QFindFD}
lsNoMaskEntered  = 'You did not enter the text to be searched.';
lsCannotSearch   = 'Cannot search';

{QFoundE, QFoundF}
lsSorting = 'Sorting';
lsDisk2 = 'Disk';
lsFreeSpace = 'Free';
lsFreeSpacePerCent = 'Free %';
lsFreeSpacePerCent1 = ' Free % ';
lsSelectionTooLarge = 'Selection too large, cannot copy to Clipboard.';
lsFolder3 = 'Folder';
lsFile = 'File';

{QI_Face}
lsErrorDBaseIsNil         = 'Internal error 500: DBaseIsNil';
lsErrorTreeStructIsNil    = 'Internal error 501: TreeStructIsNil';
lsErrorCurDirColIsNil     = 'Internal error 502: CurDirColIsNil';
lsErrorFileSearchColIsNil = 'Internal error 503: FileSearchColIsNil';
lsErrorKeyFieldIsNil      = 'Internal error 503: KeyFieldIsIsNil';
lsErrorCurFileColIsNil    = 'Internal error 504: CurFileColIsNil';
lsErrorRootDirColIsNil    = 'Internal error 505: RootDirColIsNil';
lsErrorDirColHandleIsNil  = 'Internal error 506: DirColHandleIsNil';
lsErrorFileColHandleIsNil = 'Internal error 507: FileColHandleIsNil';

{QLocOpt}
lsMask = 'Mask';
lskB = 'Bytes';
lsDLL = 'Filter';

{QMain}
lsFatalInternalError = ' (Fatal internal error ';
lsDbaseWithThisNameIsOpen = 'Database with this name is open - file cannot be overwritten.';
lsCannotCreate = 'Cannot create';
lsDBaseCannotBeCreated = 'Database cannot be created';
lsFileDoesNotExist = 'File does not exist: ';
lsErrorOnCommandLine = 'Error on command line';
lsErrorInProgramSettings = 'Error in program settings';
lsInvalidParameter = 'Invalid parameter: ';
lsOptionsCannotBeMixed = 'Options /f and /s cannot be mixed on command line.';
lsInvalidOption = 'Invalid option: ';
lsDBaseIsOpenCannotCompress =
  'Database with this name is open - it must be closed for compression.';
lsCannotCompress = 'Cannot compress';
lsCannotCompressAsItIsProtected =
  'This database cannot be compressed, because it is a read-only database.';
lsDbaseIsOpenCannotOverwrite = 'Database with this name is open - cannot be overwritten.';
lsCannotExport = 'Cannot export';
lsDBaseIsOpenMustBeClosedFirst =
  'Database with this name is open. Only closed databases can be imported.';
lsCannotImport = 'Cannot import';
lsCannotImportAsItIsProtected =
  'The database cannot be imported, because it is a read-only (protected) database.';
lsCannotImportProbablyCorrupted =
  'This database cannot be imported. It is either corrupted or requires newer version of this program.';
lsCannotContinue = 'Cannot continue';
lsDBaseIsOpenCannotRepair =
  'Database with this name is open. Only closed databases can be repaired.';
lsCannotRepairCorruptedHeader =
  'This file is either not a DiskBase database, or it has unrepairable header.';
lsCannotRepairByOldVersion =
  'The database was created by a newer version of the program. You cannot repair it by this version of the program.';
lsCannotRepairRuntimeDatabase =
  'This database cannot be repaired, as it is read-only database.';
lsDatabaseRepair = 'Database Repair';
lsRepairingIndex = 'Repairing index table...';
lsScannningDatabase = 'Scanning the database, please wait...';
lsEject = 'Eject disk from ';

{QReindex}
lsCompressingDatabase = 'Compressing database';
lsExportingToRunTime = 'Exporting to read-only form';
lsExporting = 'Exporting';
lsImporting = 'Importing';

lsCompressNotSuccessful = 'Compression was not successful';
lsExportNotSuccessful = 'Export was not successful';
lsImportNotSuccessful = 'Import was not successful';
lsSourceDBase = 'The source database ';
lsTargetDBase = 'The target database ';
lsCannotBeCreated = ' cannot be created.';
lsProcessNotSuccessful = 'Process was not successful.';

lsDBase = 'The database ';
lsCannotBeRenamedTo = ' cannot be renamed to ';
lsOriginalDBase = 'The original database ';
lsCannotBeDeleted = ' cannot be deleted.';

{QScanDrv}
lsScanArchive = 'Scan archive file ';
lsScanningTree = 'Scanning tree structure:';
lsScanningFoldersAndFiles = 'Scanning contents of folders and archives:';

{QSetting}
lsBold = ', bold';
lsItalics = ', italics';
lsArial = 'Arial';
lsMSSansSerif = 'MS Sans Serif';
lsScript = 'Default';
lsCompressionNecessary =
  'WARNING: If you change this setting, you must close and compress all DiskBase databases. See Help for details.';

{QStream}
lsFileAccessError = 'File access error.';
lsFileCreationError = 'File creation or open error.';
lsFileReadingError = 'File reading error.';
lsFileWritingError = 'File writing error.';
lsFileSeekingError = 'File seeking error.';
lsFileError = 'File error: ';

{QRegNote}
lsOk = 'OK';
lsEndProgram = 'Exit';
lsRegisterNow = 'Register now';

lsNonameFile = 'NoName.qdr';
lsExportFile = 'Export.qdr';

{QScanDir}
lsInvalidFolder = 'Invalid folder. Enter full path, including the disk letter.';

lsScanFolderAsDisk = 'Scan Folder As Disk';
lsScanFolder       = 'Scan folder:';
lsAndSaveItAsDisk  = 'and save it to database as disk:';

lsRescanDisk       = 'Rescan Disk';
lsRescanFolder     = 'Rescan disk (folder):';
lsAndSaveItAsUpdateOfDisk = 'and save it to database as update of disk:';
lsRescanWarning    =
'The disk or folder is different from the original one. This may cause a loss of manually created descriptions. Continue?';

{QCollect}
lsCollectionOverflow = 'Too large list or not enough memory.';
lsInvalidIndex       = 'Internal problem in list indexing.';
lsAbstractMethodUsed = 'Internal error: Abstract method used.';

{QFAskLab}
lsGenerated          = 'Generated name: ';

{QFExport}
lsExportOnlySelected = 'Export only selected items?'#13#10'No = export all.';
lsMultipleItemsSelected = 'Group of items is selected.';
lsFilterDatabase  = 'Export Format for Database (*.defd)|*.defd|All files (*.*)|*.*';
lsFilterFileList  = 'Export Format for Found files (*.deff)|*.deff|All files (*.*)|*.*';
lsFilterEmptyList = 'Export Format for free Space (*.defs)|*.defs|All files (*.*)|*.*';

{$endif}


implementation


end.
