unit MorrowindBSA;

interface

uses
  System.Classes, System.SysUtils, Windows, Controls, Vcl.Forms,
  System.Generics.Collections, AbstractBSAWorker;

type
  PBSAHash = ^TBSAHash;

  TBSAHash = record
    value1, value2: LongInt;
  end;

  PFileInfo = ^TFileInfo;

  TFileInfo = record
    Size: LongInt;
    Offset: LongInt;
  end;

  PFileInfoList = ^TFileInfoList;
  TFileInfoList = TArray<TFileInfo>;
  PHashList = ^THashList;
  THashList = array [0 .. 0] of TBSAHash;

  TMorrowindBSAHeader = record
    Version: integer;
    HashOffset: LongInt;
    FileCount: LongInt;
  end;

  TMorrowindFileStruct = class(TAbstractFile)
  public
    Constructor Create;
    procedure Reset; overload;
    property AbsolutePath;
    property Name;
    property Size;
    property Offset;
    property Directory;
    property LoadFlag;
    property Hash1;
    property Hash2;
    // property Hash;
  end;

  TMorrowindWorker = class(TAbstractBSAWorker)
  private
    FileList: TAbstractFileList;
    DataSection: integer;
    function MakeBSAHeader: TMorrowindBSAHeader;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure OpenArchive(AName: string); override;
    function ExtractFile(Idx: integer): Boolean; override;
    function ExtractAllFiles: Boolean; override;
    function ExtractDirectory(const DirectoryName: string): Boolean; override;
    procedure RenameFile(Idx: integer; NewName: string); override;
    procedure DeleteFile(Idx: integer); override;
    { function GenHash1(Data: TAbstractFile): int64;override;
      function GenHash2(Data: TAbstractFile): int64;override; }
    procedure GenHash(Data: TAbstractFile); override;
    procedure AddFiles(const DirectoryPath: string); override;
    procedure RefreshArchive; override;
    procedure InsertFileToArchive(Idx: integer); override;
    procedure SaveFilesToArchive(ArchiveName: string); override;
    procedure DrawFilesByDirectory(const DirectoryName: string); override;
    procedure GetFileData(const Size: integer; const Offset: integer;
      out Arr: TArray<byte>); overload; override;
    procedure GetFileData(const FilePath: string;
      out Arr: TArray<byte>); overload;
    // procedure
    property UpdateFunction;
    property ProcessFileFunction;
    property UpdateListFunction;
    property SetMaxProgressFunction;
    property CurrentProgressFunction;
    property ZeroProgressFunction;
    property ExtractionPath;
    property ArchiveName;
    property Files: TAbstractFileList read FileList write FileList;
  end;

var
  test: TAbstractBSAWorker;

implementation

uses uUtils, strutils;
{ TMorrowindFileStruct }

constructor TMorrowindFileStruct.Create;
begin
  Reset;
end;

procedure TMorrowindFileStruct.Reset;
begin
  Name := '';
  Size := -1;
  Offset := -1;
  Directory := '';
  LoadFlag := false;
  Hash1 := 0;
  Hash2 := 0;
end;

{ TMorrowindWorker }

procedure TMorrowindWorker.AddFiles(const DirectoryPath: string);
var
  Dirs: TStringList;
  AFile: TAbstractFile;
  i, len, sz: integer;
  s: string;
  tsr: tsearchrec;
begin
  inherited;
  Dirs := TStringList.Create;
  try
    GetTreeDirs(DirectoryPath, Dirs);
    for i := 0 to Dirs.Count - 1 do
    begin
      if (FindFirst(Dirs[i] + '\*.*', faAnyFile, tsr) = 0) then
      begin
        repeat
          if (tsr.Attr and faDirectory) <> faDirectory then
          begin
            AFile := TAbstractFile.Create;
            AFile.LoadFlag := True;
            AFile.AbsolutePath := Dirs[i] + '\' + tsr.Name;
            len := PosEx(DirectoryPath, AFile.AbsolutePath);
            sz := (AFile.AbsolutePath.Length - DirectoryPath.Length);
            AFile.Directory :=
              ExtractFilePath(Copy(AFile.AbsolutePath,
              len + DirectoryPath.Length + 1, sz - 1));
            AFile.Name := ExtractFileName(AFile.AbsolutePath) + #0;
            FileList.Add(AFile);
          end;
          // Files.Add(Dirs[i]+'\'+tsr.name);
        until FindNext(tsr) <> 0;
        System.SysUtils.FindClose(tsr);
      end;
    end;
    RefreshArchive;
  finally
    Dirs.Free;
  end;
end;

constructor TMorrowindWorker.Create;
begin
  ArchiveName := '';
  ExtractionPath := '';
  FileList := TAbstractFileList.Create;
  DataSection := 0;
end;

procedure TMorrowindWorker.DeleteFile(Idx: integer);
begin
  inherited;

end;

destructor TMorrowindWorker.Destroy;
begin
  FileList.Free;
  inherited;
end;

procedure TMorrowindWorker.DrawFilesByDirectory(const DirectoryName: string);
var
  i: integer;
  DirectoryStr: string;
begin
  for i := 0 to FileList.Count - 1 do
  begin
    DirectoryStr := TMorrowindFileStruct(FileList[i]).Directory;
    if CompareText(DirectoryName, DirectoryStr) = 0 then
      UpdateListFunction(TMorrowindFileStruct(FileList[i]).Name,
        TMorrowindFileStruct(FileList[i]).Offset,
        TMorrowindFileStruct(FileList[i]).Size, FileList.IndexOf(FileList[i]));

  end;

end;

function TMorrowindWorker.ExtractAllFiles: Boolean;
var
  i: integer;
begin
  inherited;
  Result := True;
  for i := 0 to Files.Count - 1 do
    Result := ExtractFile(i);
end;

function TMorrowindWorker.ExtractDirectory(const DirectoryName: string)
  : Boolean;
var
  i: integer;
  DirectoryStr: string;
begin
  Result := True;
  for i := 0 to Files.Count - 1 do
  begin
    DirectoryStr := TMorrowindFileStruct(Files[i]).Directory;
    if pos(DirectoryName, DirectoryStr) > 0 then
      Result := ExtractFile(i);
  end;

end;

function TMorrowindWorker.ExtractFile(Idx: integer): Boolean;
var
  BSA: TMorrowindFileStruct;
  FileData: TArray<byte>;
  FOutputFile: TFileStream;
  FOpenFile: TFileStream;
  Dir, Arch: string;
begin
  Result := false;
  BSA := TMorrowindFileStruct(FileList[Idx]);
  SetMaxProgressFunction(3);
  Arch := StringReplace(ExtractFileName(ArchiveName), '.', '_', [rfReplaceAll]);
  Dir := ExtractionPath + '\' + Arch + '\' + BSA.Directory;
  FOpenFile := TFileStream.Create(ArchiveName, fmShareDenyRead);
  Application.ProcessMessages;
  CurrentProgressFunction(0, 'Open ' + ArchiveName + '...');
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  FOutputFile := TFileStream.Create(Dir + '\' + BSA.Name, fmCreate);
  Application.ProcessMessages;
  CurrentProgressFunction(1, 'Create file ' + BSA.Name);
  try
    SetLength(FileData, BSA.Size);
    FOpenFile.Seek(BSA.Offset + DataSection, SoFromBeginning);
    FOpenFile.Read(FileData, BSA.Size);
    FOutputFile.Write(FileData, BSA.Size);
    Result := True;
    Application.ProcessMessages;
    CurrentProgressFunction(2, 'Completed!');
  finally
    ZeroProgressFunction;
    FOutputFile.Free;
    FOpenFile.Free;
    SetLength(FileData, 0);
  end;
end;

procedure TMorrowindWorker.InsertFileToArchive(Idx: integer);
begin
  inherited;

end;

function TMorrowindWorker.MakeBSAHeader: TMorrowindBSAHeader;
var
  Header: TMorrowindBSAHeader;
  i: integer;
  Offset: LongInt;
begin
  Offset := 0;
  Header.Version := $00000100;
  Header.FileCount := FileList.Count;
  RefreshArchive;
  Offset := ((SizeOf(TFileInfo) + SizeOf(LongWord)) * FileList.Count);
  for i := 0 to FileList.Count - 1 do
    Offset := Offset +
      Length(AnsiString(FileList[i].Directory + FileList[i].Name));
  Header.HashOffset := Offset { + SizeOf(TMorrowindBSAHeader) };
  Result := Header;
end;

Procedure TMorrowindWorker.OpenArchive(AName: string);
var
  BSAFile: TMorrowindFileStruct;
  Version, TotalFiles, HashOffset, NamesOffset, NameOffset: integer;
  FileOffset, FileSize, OffsetPos: integer;
  Hash: int64;
  i: integer;
  Name: AnsiString;
  C: AnsiChar;
  Buff: TmemoryStream;
  OpenFile: TFileStream;
  off: integer;
  Curr: TCursor;
begin
  DataSection := 0;
  Version := 0;
  HashOffset := 0;
  TotalFiles := 0;
  NamesOffset := 0;
  NameOffset := 0;
  FileList.Clear;
  ArchiveName := AName;
  OpenFile := TFileStream.Create(ArchiveName, fmShareDenyRead);
  try
    OpenFile.Read(Version, 4);
    if (Version <> 256) then
      raise Exception.Create(ErrIncorrectBSAVersion);
    OpenFile.Read(off, 4);
    HashOffset := off;
    HashOffset := HashOffset + 12;
    OpenFile.Read(TotalFiles, 4);
    SetMaxProgressFunction(TotalFiles + 2);
    OffsetPos := OpenFile.Position;
    UpdateFunction(ArchiveName, TotalFiles);
    NamesOffset := $0C + TotalFiles * 8;
    Application.ProcessMessages;
    CurrentProgressFunction(0, 'Start file data reading...');
    for i := 0 to TotalFiles - 1 do
    begin
      FileOffset := 0;
      FileSize := 0;
      BSAFile := TMorrowindFileStruct.Create;
      OpenFile.Seek(OffsetPos, SoFromBeginning);
      OpenFile.Read(FileSize, 4);
      BSAFile.Size := FileSize;
      OpenFile.Read(FileOffset, 4);
      BSAFile.Offset := FileOffset;
      OffsetPos := OpenFile.Position;
      OpenFile.Seek(NamesOffset + i * 4, SoFromBeginning);
      OpenFile.Read(NameOffset, 4);
      OpenFile.Seek(NamesOffset + TotalFiles * 4 + NameOffset, SoFromBeginning);
      Name := '';
      repeat
        OpenFile.Read(C, 1);
        Name := Name + C;
      until (byte(C) = $00);
      BSAFile.Name := ExtractFileName(Name);
      BSAFile.Directory := ExtractFilePath(Name);
      Hash := -1;
      OpenFile.Seek(HashOffset + (8 * i), SoFromBeginning);
      OpenFile.Read(Hash, 4);
      BSAFile.Hash1 := Hash;
      Hash := 0;
      OpenFile.Read(Hash, 4);
      BSAFile.Hash2 := Hash;
      HashOffset := OpenFile.Position;
      FileList.Add(BSAFile);
      Application.ProcessMessages;
      CurrentProgressFunction(i, 'Parsed: ' + IntToStr(i) + '\' +
        IntToStr(TotalFiles));
      if (i = TotalFiles - 1) then
        DataSection := (off + 12) + (8 * TotalFiles);
    end;
    CurrentProgressFunction(TotalFiles + 1, 'Reading completed!');
  finally
    ZeroProgressFunction;
    OpenFile.Free;
  end;

end;

procedure TMorrowindWorker.GenHash(Data: TAbstractFile);
var
  Name: AnsiString;
  len, sum, off, i, temp, n, l: cardinal;
begin
  Name := AnsiString(TrimLeft(Data.Directory + Data.Name));
  len := Length(Name) - 1;
  l := 0;
  l := len shr 1;
  sum := 0;
  off := 0;
  for i := 1 to l do
  begin
    sum := sum xor (cardinal(Name[i]) shl (off and $1F));
    off := off + 8;
  end;
  Data.Hash1 := sum;
  sum := 0;
  off := 0;
  temp := 0;
  n := 0;
  while i <= len do
  begin
    temp := (cardinal(Name[i]) shl (off and $1F));
    sum := sum xor temp;
    n := temp and $1F;
    sum := (sum shl (32 - n)) or (sum shr n);
    off := off + 8;
    Inc(i);
  end;
  Data.Hash2 := sum;
end;

procedure TMorrowindWorker.GetFileData(const FilePath: string;
  out Arr: TArray<byte>);
var
  FOpenFile: TFileStream;
begin
  FOpenFile := TFileStream.Create(FilePath, fmShareDenyRead);
  try
    SetLength(Arr, FOpenFile.Size);
    FOpenFile.Seek(0, SoFromBeginning);
    FOpenFile.Read(Arr, FOpenFile.Size);
  finally
    FOpenFile.Free;
  end;
end;

procedure TMorrowindWorker.GetFileData(const Size, Offset: integer;
  out Arr: TArray<byte>);
var
  FOpenFile: TFileStream;
  Dir, Arch: string;
begin
  Arch := StringReplace(ExtractFileName(ArchiveName), '.', '_', [rfReplaceAll]);
  // Dir := ExtractionPath + '\' + Arch + '\' + BSA.Directory;
  FOpenFile := TFileStream.Create(ArchiveName, fmShareDenyRead);
  try
    SetLength(Arr, Size);
    FOpenFile.Seek(Offset + DataSection, SoFromBeginning);
    FOpenFile.Read(Arr, Size);
  finally
    FOpenFile.Free;
  end;
end;

procedure TMorrowindWorker.RefreshArchive;
  function GetFileSize(const FileName: string): int64;
  var
    Handle: THandle;
    FindData: TWin32FindData;
  begin
    Handle := FindFirstFile(PChar(FileName), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        Int64Rec(Result).Lo := FindData.nFileSizeLow;
        Int64Rec(Result).Hi := FindData.nFileSizeHigh;
        Exit;
      end;
    end;
    Result := -1;
  end;

var
  i: integer;
  Offset: integer;
  AFile: TMorrowindFileStruct;
begin
  Offset := 0;
  FileList.Sort(CompareByName);
  for i := 0 to FileList.Count - 1 do
  begin
    AFile := TMorrowindFileStruct(FileList[i]);
    if AFile.LoadFlag then
    begin
      AFile.Size := GetFileSize(AFile.AbsolutePath);
      AFile.Offset := Offset;
      GenHash(AFile);
    end
    else
      GenHash(AFile);
    Offset := Offset + AFile.Size;
  end;
  FileList.Sort(CompareByHash);
end;

procedure TMorrowindWorker.RenameFile(Idx: integer; NewName: string);
begin
  inherited;

end;

procedure TMorrowindWorker.SaveFilesToArchive(ArchiveName: string);
var
  Header: TMorrowindBSAHeader;
  M: byte;
  i, J: integer;
  Buffer: TArray<byte>;
  filenameOffset: integer;
  FileNameOffsets: TArray<integer>;
  Hashes: TArray<TBSAHash>;
  FS: TFileStream;
  FInfoList: TFileInfoList;
  Names: TArray<AnsiString>;
begin
  inherited;
  FileNameOffsets := 0;
  Header := MakeBSAHeader;
  filenameOffset := 0;
  try
    FS := TFileStream.Create(ArchiveName, fmCreate);
    FS.Write(@Header, SizeOf(Header));
    SetLength(FInfoList, Header.FileCount * SizeOf(TFileInfo));
    SetLength(Hashes, Header.FileCount * SizeOf(TBSAHash));
    SetLength(FileNameOffsets, Header.FileCount * SizeOf(integer));
    SetLength(Names, Header.FileCount);
    FileList.Sort(CompareByHash);
    for i := 0 to Header.FileCount - 1 do
    begin
      FInfoList[i].Offset := FileList[i].Offset;
      FInfoList[i].Size := FileList[i].Size;
      Names[i] := AnsiString(FileList[i].Directory + FileList[i].Name);
      FileNameOffsets[i] := filenameOffset;
      filenameOffset := filenameOffset +
        Length(AnsiString(FileList[i].Directory + FileList[i].Name));
      Hashes[i].value1 := FileList[i].Hash1;
      Hashes[i].value2 := FileList[i].Hash2;
    end;
    FS.Write(@FInfoList[0], Header.FileCount * SizeOf(TFileInfo));
    FS.Write(@FileNameOffsets[0], Header.FileCount * SizeOf(integer));
    for J := 0 to Length(Names) - 1 do
    begin
      FS.Write(Names[J][1], Length(Names[J]) * SizeOf(AnsiChar));
    end;
    FS.Write(@Hashes[0], Header.FileCount * SizeOf(TBSAHash));
    FileList.Sort(CompareByName);
    for i := 0 to Header.FileCount - 1 do
    begin
      if FileList[i].LoadFlag then
        GetFileData(FileList[i].AbsolutePath, Buffer)
      else
        GetFileData(FileList[i].Size, FileList[i].Offset, Buffer);
      FS.Write(Buffer, FileList[i].Size);
      SetLength(Buffer, 0);
    end;
  finally
    SetLength(FInfoList, 0);
    SetLength(Names, 0);
    SetLength(Hashes, 0);
    SetLength(FileNameOffsets, 0);
    FS.Free;
  end;
end;

end.
