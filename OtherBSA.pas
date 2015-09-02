unit OtherBSA;

interface

uses
  System.Classes, System.SysUtils, AbstractBSAWorker, Generics.Defaults,
  Generics.Collections, Forms, Zlib;

const
  BSASignature: array [0 .. 3] of ansichar = ('B', 'S', 'A', #0);

type
  TBSAHeader = record
    fileId: array [0 .. 3] of ansichar;
    version: Cardinal;
    offset: Cardinal;
    archiveFlags: Cardinal;
    folderCount: Cardinal;
    fileCount: Cardinal;
    totalFolderNameLength: Cardinal;
    totalFileNameLength: Cardinal;
    fileFlags: Cardinal;
  end;

  TBSAFolder = record
    nameHash: Array [0 .. 1] of Cardinal;
    count: Cardinal;
    offset: Cardinal;
  end;

  TBSAFile = record
    nameHash: Array [0 .. 1] of Cardinal;
    size: Cardinal;
    offset: Cardinal;
    // folder_id : Cardinal;
  end;

  TBSAFileRecords = TArray<TBSAFile>;

  TBSAFolderBlock = record
    name: TArray<ansichar>;
    filesRecords: TBSAFileRecords;
  end;

  TBSAFoldersBlock = TArray<TBSAFolderBlock>;

  TBSAFolders = TArray<TBSAFolder>;

  TBSAFileStruct = class(TAbstractFile)
  private
    FCompressionFlag: integer;
    FCompressedSize: integer;
  public
    Constructor Create;
    procedure Reset; overload;
    property CompressionFlag: integer read FCompressionFlag
      write FCompressionFlag;
    Property CompressedSize: integer read FCompressedSize write FCompressedSize;
    property Name;
    property size;
    property offset;
    property OldOffset;
    property Directory;
    property LoadFlag;
    property Hash1;
    property Hash2;
  end;

  TBSAFileRecord = class
  private
    FFilePath: AnsiString;
    FSize: LongWord;
    FName: AnsiString;
    FHash: LongInt;
    FOffsetPos: Int64;
    FDoCompress: boolean;
    FHDDFlag: boolean;
    procedure Clear;
  public
    constructor Create;
    procedure WriteName(Stream: TStream);
    property FilePath: AnsiString read FFilePath write FFilePath;
    property size: LongWord read FSize write FSize;
    property Name: AnsiString read FName write FName;
    property Hash: LongInt read FHash write FHash;
    property OffsetPos: Int64 read FOffsetPos write FOffsetPos;
    property DoCompress: boolean read FDoCompress write FDoCompress;
    property HDDFlag: boolean read FHDDFlag write FHDDFlag;
  end;

  TBSAFolderRecord = class
  private
    FName: AnsiString;
    FHash: LongInt;
    FOffsetPos: Int64;
    FFiles: TList<TBSAFileRecord>;
    procedure Clear;
  public
    Constructor Create;
    destructor Destroy; override;
    function GetNameLength: integer;
    function GetTotalFileNameLength: integer;
    function GetTotalFilesSize: integer;
    property Name: AnsiString read FName write FName;
    property Hash: LongInt read FHash write FHash;
    property OffsetPos: Int64 read FOffsetPos write FOffsetPos;
    property Files: TList<TBSAFileRecord> read FFiles;
  end;

  TBSAWritter = class
  private
    FFolders: TList<TBSAFolderRecord>;
    FHeader: TBSAHeader;
    FCurrentOffset: Cardinal;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    property Folders: TList<TBSAFolderRecord> read FFolders;
    property Header: TBSAHeader read FHeader write FHeader;
    property CurrentOffset: Cardinal read FCurrentOffset write FCurrentOffset;
  end;

  TBSAWorker = class(TAbstractBSAWorker)
  private
    FHeader: TBSAHeader;
    FFileList: TAbstractFileList;
    FDataSection: integer;
    FFileNameOffset: integer;
    // FWritingTree: TTree<T>;
    procedure Clear;
    function MakeHashString(str: string): LongInt;
    function GenHash(FileName: string; Ext: string): LongInt;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure OpenArchive(AName: string); override;
    function ExtractFile(Idx: integer): boolean; override;
    function ExtractAllFiles: boolean; override;
    function ExtractDirectory(const DirectoryName: string): boolean; override;
    procedure RenameFile(Idx: integer; NewName: string); override;
    procedure DeleteFile(Idx: integer); override;
    procedure InsertFileToArchive(Idx: integer); override;
    procedure SaveFilesToArchive(ArciveName: string); override;
    procedure DrawFilesByDirectory(const DirectoryName: string); override;
    // procedure
    property UpdateFunction;
    property ProcessFileFunction;
    property UpdateListFunction;
    property SetMaxProgressFunction;
    property CurrentProgressFunction;
    property ZeroProgressFunction;
    property ExtractionPath;
    property ArchiveName;
    property DataSection: integer read FDataSection write FDataSection;
    property FileNameOffset: integer read FFileNameOffset write FFileNameOffset;
    property Files: TAbstractFileList read FFileList write FFileList;
  end;

implementation

uses strutils;
{ TBSAWorker }

procedure TBSAWorker.Clear;
begin
  FDataSection := 0;
  FFileList.Clear;
end;

constructor TBSAWorker.Create;
begin
  FFileList := TAbstractFileList.Create;
  Clear;
end;

procedure TBSAWorker.DeleteFile(Idx: integer);
begin
  inherited;

end;

destructor TBSAWorker.Destroy;
begin
  Clear;
  FFileList.Free;
  inherited;
end;

procedure TBSAWorker.DrawFilesByDirectory(const DirectoryName: string);
var
  i: integer;
  DirectoryStr: string;
begin
  for i := 0 to Files.count - 1 do
  begin
    DirectoryStr := TBSAFileStruct(Files[i]).Directory;
    if CompareText(DirectoryName, DirectoryStr) = 0 then
      UpdateListFunction(TBSAFileStruct(Files[i]).name, TBSAFileStruct(Files[i])
        .offset, TBSAFileStruct(Files[i]).size, Files.IndexOf(Files[i]));
  end;
end;

function TBSAWorker.ExtractAllFiles: boolean;
var
  i: integer;
begin
  inherited;
  Result := true;
  for i := 0 to Files.count - 1 do
    Result := ExtractFile(i);
end;

function TBSAWorker.ExtractDirectory(const DirectoryName: string): boolean;
var
  i: integer;
  DirectoryStr: string;
begin
  Result := true;
  for i := 0 to Files.count - 1 do
  begin
    DirectoryStr := TBSAFileStruct(Files[i]).Directory;
    if pos(DirectoryName, DirectoryStr) > 0 then
      Result := ExtractFile(i);
  end;
end;

function TBSAWorker.ExtractFile(Idx: integer): boolean;
var
  BSA: TBSAFileStruct;
  FileData: TArray<Byte>;
  FOutputFile: TFileStream;
  FOpenFile: TFileStream;
  Dir, Arch: string;
  Zip: TZDecompressionStream;
  Buffer: TMemoryStream;
begin
  Result := false;
  BSA := TBSAFileStruct(Files[Idx]);
  Arch := StringReplace(ExtractFileName(ArchiveName), '.', '_', [rfReplaceAll]);
  Dir := ExtractionPath + '\' + Arch + '\' + BSA.Directory;
  FOpenFile := TFileStream.Create(ArchiveName, fmShareDenyRead);
  SetMaxProgressFunction(3);
  Application.ProcessMessages;
  CurrentProgressFunction(0, 'Open ' + ArchiveName + '...');
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  FOutputFile := TFileStream.Create(Dir + '\' + BSA.name, fmCreate);
  Application.ProcessMessages;
  CurrentProgressFunction(1, 'Create file ' + BSA.name);
  try
    SetLength(FileData, BSA.size);
    FOpenFile.Seek(BSA.offset, SoFromBeginning);
    if (BSA.CompressionFlag > 0) then
    begin
      try
        Buffer := TMemoryStream.Create;
        FOpenFile.Seek(4, soFromCurrent);
        Buffer.CopyFrom(FOpenFile, BSA.size - 4);
        Buffer.Seek(0, SoFromBeginning);
        Zip := TZDecompressionStream.Create(Buffer);
        Buffer.Seek(0, SoFromBeginning);
        FOutputFile.CopyFrom(Zip, 0);
      finally
        Zip.Free;
        Buffer.Free;
      end;
    end
    else
    begin
      FOpenFile.Read(FileData, BSA.size);
      FOutputFile.Write(FileData, BSA.size);
    end;
    Result := true;
    Application.ProcessMessages;
    CurrentProgressFunction(2, 'Completed!');
  finally
    FOutputFile.Free;
    FOpenFile.Free;
    SetLength(FileData, 0);
  end;
end;

function TBSAWorker.GenHash(FileName, Ext: string): LongInt;
var
  res1, res2, res3: LongInt;
  len: integer;
begin
  res1 := 0;
  res2 := 0;
  res3 := 0;
  len := FileName.Length - 1;
  if len > 0 then
  begin
    res1 := LongInt(Byte(FileName[len]) + (len shl 16) + Byte(FileName[1]));
    if len > 2 then
      res1 := res1 + (Byte(FileName[len - 1]) shl 8);
    if len > 3 then
      res2 := MakeHashString(Copy(FileName, 2, len - 2));
  end;
  case IndexStr(Ext, ['.kf', '.nif', '.dds', '.wav']) of
    0:
      res1 := res1 + $80;
    1:
      res1 := res1 + $8000;
    2:
      res1 := res1 + $8080;
    3:
      res1 := res1 + $80000000
  end;
  res3 := MakeHashString(Ext);
  res2 := res2 + res3;
  Result := LongInt((res2 shl 32) + res1);
end;

procedure TBSAWorker.InsertFileToArchive(Idx: integer);
begin
  inherited;

end;

function TBSAWorker.MakeHashString(str: string): LongInt;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to str.Length - 1 do
    Result := Result * $1003F + Byte(str[i]);
end;

procedure TBSAWorker.OpenArchive(AName: string);
var
  BSAFile: TBSAFileStruct;
  i, j: integer;
  name, TempDirectory: AnsiString;
  OpenFile: TFileStream;
  Folders: TBSAFolders;
  FileRecs: TBSAFileRecords;
  CharIndex: integer;
  CurFile: integer;
  ReadByte: Byte;
  C: ansichar;
  FolderPathLen: Cardinal;
  FoldersBlocks: TBSAFoldersBlock;
begin
  Clear;
  CurFile := 0;
  ArchiveName := AName;
  OpenFile := TFileStream.Create(ArchiveName, fmShareDenyRead);
  try
    OpenFile.Read(FHeader, SizeOf(TBSAHeader));
    UpdateFunction(ArchiveName, FHeader.fileCount);
    SetMaxProgressFunction(FHeader.folderCount);
    SetLength(Folders, FHeader.folderCount);
    OpenFile.Read(Folders[0], SizeOf(TBSAFolder) * FHeader.folderCount);
    SetLength(FoldersBlocks, FHeader.folderCount);
    for i := 0 to FHeader.folderCount - 1 do
    begin
      OpenFile.Read(ReadByte, SizeOf(Byte));
      FolderPathLen := ord(ReadByte);
      SetLength(FoldersBlocks[i].name, FolderPathLen);
      for CharIndex := 0 to FolderPathLen - 1 do
      begin
        OpenFile.Read(ReadByte, SizeOf(Byte));
        FoldersBlocks[i].name[CharIndex] := ansichar(chr(ReadByte));
      end;
      SetString(TempDirectory, PAnsiChar(FoldersBlocks[i].name), FolderPathLen);
      Delete(TempDirectory, Length(TempDirectory), 1);
      TempDirectory := TempDirectory + '\';
      SetLength(FoldersBlocks[i].filesRecords, Folders[i].count);
      for j := 0 to Folders[i].count - 1 do
      begin
        BSAFile := TBSAFileStruct.Create;
        OpenFile.Read(FoldersBlocks[i].filesRecords[j], SizeOf(TBSAFile));
        with BSAFile do
        begin
          size := FoldersBlocks[i].filesRecords[j].size;
          offset := FoldersBlocks[i].filesRecords[j].offset;
          Directory := TempDirectory;
          Hash1 := FoldersBlocks[i].filesRecords[j].nameHash[0];
          Hash2 := FoldersBlocks[i].filesRecords[j].nameHash[1];
          if (FHeader.archiveFlags and $004) > 0 then
          begin
            CompressionFlag := 1;
            CompressedSize := size;
          end;
          if (size and (1 shl 30)) <> 0 then
          begin
            size := size xor (1 shl 30);
            CompressionFlag := 1;
          end;
        end;
        Files.Add(BSAFile);
      end;
      Application.ProcessMessages;
      CurrentProgressFunction(i, 'Folder processed!');
    end;
    FFileNameOffset := OpenFile.Position;
    for i := 0 to FHeader.folderCount - 1 do
    begin
      for j := 0 to Folders[i].count - 1 do
      begin
        Name := '';
        repeat
          OpenFile.Read(C, 1);
          Name := Name + C;
        until (Byte(C) = $00);
        Files[CurFile].name := Name;
        Inc(CurFile);
      end;
    end;
  finally
    CurrentProgressFunction(FHeader.folderCount, 'Reading completed!');
    ZeroProgressFunction;
    Folders := nil;
    FileRecs := nil;
    FoldersBlocks := nil;
    OpenFile.Free;
  end;
end;

procedure TBSAWorker.RenameFile(Idx: integer; NewName: string);
begin
  inherited;

end;

procedure TBSAWorker.SaveFilesToArchive(ArciveName: string);
begin
  inherited;

end;

{ TBSAFileStruct }

constructor TBSAFileStruct.Create;
begin
  Reset;
end;

procedure TBSAFileStruct.Reset;
begin
  Name := '';
  size := -1;
  offset := -1;
  Directory := '';
  LoadFlag := false;
  Hash1 := 0;
  Hash2 := 0;
  CompressionFlag := -1;
end;

{ TBSAFolderRecord }

procedure TBSAFolderRecord.Clear;
begin
  FOffsetPos := -1;
  FHash := -1;
  FName := '';
end;

constructor TBSAFolderRecord.Create;
begin
  FFiles := TList<TBSAFileRecord>.Create;
  Clear;
end;

destructor TBSAFolderRecord.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TBSAFolderRecord.GetNameLength: integer;
begin
  Result := Length(Name);
end;

function TBSAFolderRecord.GetTotalFileNameLength: integer;
var
  sum: integer;
  i: integer;
begin
  sum := 0;
  for i := 0 to Files.count - 1 do
    sum := Length(Files[i].name);
  Result := sum;
end;

function TBSAFolderRecord.GetTotalFilesSize: integer;
var
  sum: integer;
  i: integer;
begin
  sum := 0;
  for i := 0 to Files.count - 1 do
    sum := Files[i].size;
  Result := sum;
end;

{ TBSAFileRecord }

procedure TBSAFileRecord.Clear;
begin
  FFilePath := '';
  FSize := 0;
  FName := '';
  FHash := -1;
  FOffsetPos := -1;
  FDoCompress := false;
  FHDDFlag := false;
end;

constructor TBSAFileRecord.Create;
begin
  Clear;
end;

procedure TBSAFileRecord.WriteName(Stream: TStream);
begin
  Stream.Write(Name[1], Length(Name) * SizeOf(ansichar));
end;

{ TBSAWritter }

procedure TBSAWritter.Clear;
begin
  Folders.Clear;
  CurrentOffset := 0;
  with Header do
  begin
    {version := 0;
    offset := 0;
    archiveFlags := 0;
    folderCount := 0;
    fileCount := 0;
    totalFolderNameLength := 0;
    totalFileNameLength := 0;
    fileFlags := 0;  }
  end;
end;

constructor TBSAWritter.Create;
begin
  FFolders := TList<TBSAFolderRecord>.Create;
  Clear;
end;

destructor TBSAWritter.Destroy;
begin
  Folders.Free;
  inherited;
end;

end.
