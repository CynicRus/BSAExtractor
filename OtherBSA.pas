unit OtherBSA;

interface
  uses
   System.Classes,System.SysUtils,AbstractBSAWorker,Forms,Zlib;
  type
  TBSASignature = array[0..3] of ansichar;

  TBSAHeader = record
    fileId : TBSASignature;
    version : Cardinal;
    offset : Cardinal;
    archiveFlags : Cardinal;
    folderCount : Cardinal;
    fileCount : Cardinal;
    totalFolderNameLength : Cardinal;
    totalFileNameLength : Cardinal;
    fileFlags : Cardinal;
  end;


 TBSAFolder = record
    nameHash : Array[0..1] of Cardinal;
    count : Cardinal;
    offset : Cardinal;
  end;

  TBSAFile = record
    nameHash : Array[0..1] of Cardinal;
    size : Cardinal;
    offset : Cardinal;
    //folder_id : Cardinal;
  end;

  TBSAFileRecords = TArray<TBSAFile>;

  TBSAFolderBlock = record
    name : TArray<AnsiChar>;
    filesRecords : TBSAFileRecords;
  end;

  TBSAFoldersBlock = TArray<TBSAFolderBlock>;


  TBSAFolders = TArray<TBSAFolder>;

   TBSAFileStruct = class(TAbstractFile)
    private
      FCompressionFlag: integer;
      FCompressedSize: integer;
    public
      Constructor Create;
      procedure Reset;overload;
      property CompressionFlag: integer read FCompressionFlag write FCompressionFlag;
      Property CompressedSize: Integer read FCompressedSize write FCompressedSize;
      property Name;
      property Size;
      property Offset;
      property Directory;
      property LoadFlag;
      property Hash1;
      property Hash2;
  end;

  TBSAWorker = class(TAbstractBSAWorker)
   private
    FHeader: TBSAHeader;
    FFileList: TAbstractFileList;
    FDataSection: integer;
    FFileNameOffset: integer;
    procedure Clear;
   public
      Constructor Create;
      Destructor Destroy;override;
      procedure OpenArchive(AName: string);override;
      function ExtractFile(Idx: integer): Boolean;override;
      function ExtractAllFiles : Boolean;override;
      function ExtractDirectory(const DirectoryName: string): Boolean;override;
      procedure RenameFile(Idx: integer;NewName: string);override;
      procedure DeleteFile(Idx: integer);override;
      procedure InsertFileToArchive(Idx: integer);override;
      procedure SaveFilesToArchive(ArciveName: string);override;
      procedure DrawFilesByDirectory(const DirectoryName: string);override;
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

{ TBSAWorker }

procedure TBSAWorker.Clear;
begin
  FDataSection:=0;
  FFileList.Clear;
end;

constructor TBSAWorker.Create;
begin
  FFileList:=TAbstractFileList.Create;
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
 for i := 0 to Files.Count - 1 do
   begin
     DirectoryStr:=TBSAFileStruct(Files[i]).Directory;
     if CompareText(DirectoryName,DirectoryStr) = 0 then
        UpdateListFunction(TBSAFileStruct(Files[i]).Name,TBSAFileStruct(Files[i]).Offset,TBSAFileStruct(Files[i]).size,Files.IndexOf(Files[i]));

   end;

end;

function TBSAWorker.ExtractAllFiles: Boolean;
var
 i: integer;
begin
  inherited;
  Result:=true;
  for i := 0 to Files.Count - 1 do
    Result:=ExtractFile(i);
end;

function TBSAWorker.ExtractDirectory(const DirectoryName: string): Boolean;
var
 i: integer;
 DirectoryStr: string;
begin
 result:=true;
 for i := 0 to Files.Count - 1 do
   begin
     DirectoryStr:=TBSAFileStruct(Files[i]).Directory;
     if pos(DirectoryName,DirectoryStr) > 0 then
        Result:=ExtractFile(i);

   end;

end;

function TBSAWorker.ExtractFile(Idx: integer): Boolean;
var
 BSA: TBSAFileStruct;
 FileData: TArray<Byte>;
 FOutputFile: TFileStream;
 FOpenFile: TFileStream;
 Dir,Arch: string;
 Zip: TZDecompressionStream;
 Buffer: TMemoryStream;
begin
 result:=false;
  BSA:=TBSAFileStruct(Files[idx]);


  Arch:=StringReplace(ExtractFileName(ArchiveName),'.','_',[rfReplaceAll]);
  Dir:=ExtractionPath +'\'+Arch+'\'+ BSA.Directory;
  FOpenFile:=TFileStream.Create(ArchiveName,fmShareDenyRead);
  SetMaxProgressFunction(3);
  Application.ProcessMessages;
  CurrentProgressFunction(0,'Open '+ArchiveName+'...');
  if not DirectoryExists(Dir) then
   ForceDirectories(Dir);
  FOutputFile:=TfileStream.Create(Dir+'\'+BSA.Name,fmCreate);
  Application.ProcessMessages;
  CurrentProgressFunction(1,'Create file '+BSA.Name);
  try
    SetLength(FileData,BSA.Size);
    FOpenFile.Seek(BSA.Offset,SoFromBeginning);
    if (BSA.CompressionFlag > 0) then
     begin
      try
       Buffer:=TMemoryStream.Create;
       FOpenFile.Seek(4,soFromCurrent);
       Buffer.CopyFrom(FOpenFile,BSA.Size-4);
       Buffer.Seek(0,soFromBeginning);
       Zip:=TZDecompressionStream.Create(Buffer);
       Buffer.Seek(0,soFromBeginning);
       FOutputFile.CopyFrom(Zip,0);
      finally
        Zip.Free;
        Buffer.Free;
      end;
     end else
     begin
      FOpenFile.Read(FileData,BSA.Size);
      FOutputFile.Write(FileData,BSA.Size);
     end;
    Result:=True;
    Application.ProcessMessages;
    CurrentProgressFunction(2,'Completed!');
   // ShowMessage('Exctracted: '+IntToStr(Idx)+BSA.Name+'!');
  finally
    FOutputfile.Free;
    FOpenFile.Free;
    Setlength(Filedata,0);
  end;
end;

procedure TBSAWorker.InsertFileToArchive(Idx: integer);
begin
  inherited;

end;

procedure TBSAWorker.OpenArchive(AName: string);
var
 BSAFile: TBSAFileStruct;
 i,j: integer;
 Name,TempDirectory: AnsiString;
 OpenFile: TFileStream;
 Folders: TBSAFolders;
 FileRecs: TBSAFileRecords;
 CharIndex : Integer;
 CurFile: integer;
 ReadByte : Byte;
 C:AnsiChar;
 FolderPathLen : Cardinal;
 FoldersBlocks: TBSAFoldersBlock;
begin
 Clear;
 CurFile:=0;
 ArchiveName:=AName;
 OpenFile:=TFileStream.Create(ArchiveName,fmShareDenyRead);
 try
   OpenFile.Read(FHeader,SizeOf(TBSAHeader));
   if (FHeader.version <> 103) and (FHeader.version <> 104) then
    raise Exception.Create(ErrIncorrectBSAVersion);
   UpdateFunction(ArchiveName,FHeader.fileCount);
   SetMaxProgressFunction(FHeader.folderCount);
   SetLength(Folders, FHeader.folderCount);
   OpenFile.Read(Folders[0],SizeOf(TBSAFolder)*FHeader.folderCount);
   SetLength(FoldersBlocks,FHeader.folderCount);
   for I := 0 to FHeader.folderCount - 1 do
    begin
      OpenFile.Read(readByte, SizeOf(Byte));
      folderPathLen := ord(readByte);
      SetLength(FoldersBlocks[i].name, folderPathLen);

       for charIndex := 0 to folderPathLen - 1 do
       begin
         OpenFile.Read(readByte, sizeof(byte));
         FoldersBlocks[i].name[charIndex] := AnsiChar(chr(readByte));
       end;

       SetString(TempDirectory,PAnsiChar(FoldersBlocks[i].name),FolderPathLen);
        Delete(TempDirectory, Length(TempDirectory), 1);
       TempDirectory:=TempDirectory + '\';
       SetLength(FoldersBlocks[i].filesRecords,Folders[i].count);
       for j := 0 to Folders[i].count - 1 do
        begin
          BSAFile:=TBSAFileStruct.Create;
          OpenFile.Read( FoldersBlocks[i].filesRecords[j],
           SizeOf(TBSAFile));
          with BSAFile do
          begin
            Size:= FoldersBlocks[i].filesRecords[j].size;
            Offset:= FoldersBlocks[i].filesRecords[j].offset;
            Directory:=TempDirectory;
            Hash1:=FoldersBlocks[i].filesRecords[j].nameHash[0];
            Hash2:=FoldersBlocks[i].filesRecords[j].nameHash[1];
            if (FHeader.archiveFlags and $004) > 0 then
             begin
              CompressionFlag:=1;
              CompressedSize:=Size;
             end;
             if (Size and (1 shl 30)) <> 0 then
               begin
                 Size:= Size xor (1 shl 30);
                 CompressionFlag:=1;
               end;

          end;
          Files.Add(BSAFile);
        end;
      Application.ProcessMessages;
      CurrentProgressFunction(i,'Folder processed!');

    end;

    FFileNameOffset:=OpenFile.Position;

    for i := 0 to FHeader.folderCount - 1 do
      begin
       for j := 0 to Folders[i].count - 1 do
         begin
          Name:='';
          repeat
            OpenFile.Read(C,1);
            Name:=Name+C;
          until (Byte(C) = $00);
          Files[CurFile].Name:=Name;
          Inc(CurFile);
         end;
      end;
 finally
   CurrentProgressFunction(FHeader.folderCount,'Reading completed!');
   ZeroProgressFunction;
   Folders:=nil;
   FileRecs:=nil;
   FoldersBlocks:=nil;
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
  Name:='';
  Size:=-1;
  Offset:=-1;
  Directory := '';
  LoadFlag := false;
  Hash1 := -1;
  Hash2 := -1;
  CompressionFlag := -1;
end;

end.
