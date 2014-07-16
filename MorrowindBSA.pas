unit MorrowindBSA;

interface
  uses
   System.Classes,System.SysUtils,Windows,Controls,Vcl.Forms,AbstractBSAWorker;
 type
  TMorrowindFileStruct = class(TAbstractFile)
    public
      Constructor Create;
      procedure Reset;overload;
      property Name;
      property Size;
      property Offset;
      property Directory;
      property LoadFlag;
      property Hash1;
      property Hash2;
  end;

  TMorrowindWorker = class(TAbstractBSAWorker)
   private
    FileList: TAbstractFileList;
    DataSection: integer;
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
      procedure SaveFilesToArchive(ArchiveName: string);override;
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
      property Files: TAbstractFileList read FileList write FileList;
  end;
implementation

{ TMorrowindFileStruct }

constructor TMorrowindFileStruct.Create;
begin
 Reset;
end;

procedure TMorrowindFileStruct.Reset;
begin
  Name:='';
  Size:=-1;
  Offset:=-1;
  Directory := '';
  LoadFlag := false;
  Hash1 := -1;
  Hash2 := -1;
end;

{ TMorrowindWorker }

constructor TMorrowindWorker.Create;
begin
  ArchiveName:='';
  ExtractionPath:='';
  FileList:=TAbstractFileList.Create;
  DataSection:=0;
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
     DirectoryStr:=TMorrowindFileStruct(FileList[i]).Directory;
     if CompareText(DirectoryName,DirectoryStr) = 0 then
        UpdateListFunction(TMorrowindFileStruct(FileList[i]).Name,TMorrowindFileStruct(FileList[i]).Offset,TMorrowindFileStruct(FileList[i]).size,FileList.IndexOf(FileList[i]));

   end;

end;

function TMorrowindWorker.ExtractAllFiles: boolean;
var
 i: integer;
begin
  inherited;
  Result:=true;
  for i := 0 to Files.Count - 1 do
    Result:=ExtractFile(i);
end;

function TMorrowindWorker.ExtractDirectory(
  const DirectoryName: string): Boolean;
var
 i: integer;
 DirectoryStr: string;
begin
 result:=true;
 for i := 0 to Files.Count - 1 do
   begin
     DirectoryStr:=TMorrowindFileStruct(Files[i]).Directory;
     if pos(DirectoryName,DirectoryStr) > 0 then
        Result:=ExtractFile(i);

   end;

end;

function TMorrowindWorker.ExtractFile(Idx: integer): boolean;
var
 BSA: TMorrowindFileStruct;
 FileData: TArray<Byte>;
 FOutputFile: TFileStream;
 FOpenFile: TFileStream;
 Dir,Arch: string;
begin
 result:=false;
  BSA:=TMorrowindFileStruct(FileList[idx]);
  SetMaxProgressFunction(3);
  Arch:=StringReplace(ExtractFileName(ArchiveName),'.','_',[rfReplaceAll]);
  Dir:=ExtractionPath +'\'+Arch+'\'+ BSA.Directory;
  FOpenFile:=TFileStream.Create(ArchiveName,fmShareDenyRead);
  Application.ProcessMessages;
  CurrentProgressFunction(0,'Open '+ArchiveName+'...');

  if not DirectoryExists(Dir) then
   ForceDirectories(Dir);
  FOutputFile:=TfileStream.Create(Dir+'\'+BSA.Name,fmCreate);
  Application.ProcessMessages;
  CurrentProgressFunction(1,'Create file '+BSA.Name);
  try
    SetLength(FileData,BSA.Size);
    FOpenFile.Seek(BSA.Offset+DataSection,SoFromBeginning);
    FOpenFile.Read(FileData,BSA.Size);
    FOutputFile.Write(FileData,BSA.Size);
    Result:=True;
    Application.ProcessMessages;
    CurrentProgressFunction(2,'Completed!');
   // ShowMessage('Exctracted: '+IntToStr(Idx)+BSA.Name+'!');
  finally
    ZeroProgressFunction;
    FOutputfile.Free;
    FOpenFile.Free;
    Setlength(Filedata,0);
  end;
end;

procedure TMorrowindWorker.InsertFileToArchive(Idx: integer);
begin
  inherited;

end;

procedure TMorrowindWorker.OpenArchive(AName: string);
var
 BSAFile: TMorrowindFileStruct;
 Version,TotalFiles,hashOffset,NamesOffset,NameOffset: integer;
 FileOffset,FileSize,OffsetPos: integer;
 Hash: int64;
 I: integer;
 Name: AnsiString;
 C: AnsiChar;
 Buff: TmemoryStream;
 OpenFile: TFileStream;
 off: integer;
 Curr: TCursor;
begin
 DataSection:=0;
 Version:=0;
 hashOffset:=0;
 TotalFiles:=0;
 NamesOffset:=0;
 NameOffset:=0;
 FileList.Clear;
 ArchiveName:=AName;
 OpenFile:=TFileStream.Create(ArchiveName,fmShareDenyRead);
 try
   OpenFile.Read(Version,4);
   if (Version <> 256) then
    raise Exception.Create(ErrIncorrectBSAVersion);
   OpenFile.Read(Off,4);
   hashOffset:=off;
   hashOffset:=HashOffset+12;
   OpenFile.Read(TotalFiles,4);
   SetMaxProgressFunction(TotalFiles+2);
   OffsetPos:=OpenFile.Position;
   UpdateFunction(ArchiveName,TotalFiles);
   NamesOffset:=$0C+TotalFiles*8;
   Application.ProcessMessages;
   CurrentProgressFunction(0,'Start file data reading...');
   for i := 0 to TotalFiles - 1 do
     begin
       FileOffset:=0;
       Filesize:=0;
      BSAFile:=TMorrowindFileStruct.Create;
      OpenFile.Seek(OffsetPos,soFromBeginning);
      OpenFile.Read(FileSize,4);
      BSAFile.Size:=FileSize;
      OpenFile.Read(FileOffset,4);
      BSAFile.Offset:=FileOffset;
     // OpenFile.Read(NameOffset,4);
      OffsetPos:=OpenFile.Position;

      OpenFile.Seek(NamesOffset+i*4,soFromBeginning);
      OpenFile.Read(NameOffset,4);
      OpenFile.Seek(NamesOffset+TotalFiles*4+NameOffset,soFromBeginning);
      Name:='';
      repeat
        OpenFile.Read(C,1);
        Name:=Name+C;
      until (Byte(C) = $00);
      BSAFile.Name:=ExtractFileName(Name);
      BSAFile.Directory:=ExtractFilePath(Name);
      Hash:=-1;
      OpenFile.Seek(hashOffset+(8 * i),soFromBeginning);
      OpenFile.Read(Hash,8);
      BSAFile.Hash1:=Hash;
      Hash:=0;
      OpenFile.Read(Hash,8);
      BSAFile.Hash2:=Hash;   
      hashOffset:=OpenFile.Position;
      FileList.Add(BSAFile);
      Application.ProcessMessages;
      CurrentProgressFunction(i,'Parsed: '+IntToStr(i)+'\'+IntToStr(TotalFiles));
    //  UpdateListFunction(BSAFile.Name,BSAFile.Offset,BSAFile.Size);
      if (i = TotalFiles - 1) then
       DataSection:= (off+12)+(8 * TotalFiles);
     end;
     CurrentProgressFunction(TotalFiles+1,'Reading completed!');
 finally
   ZeroProgressFunction;
  // setSystemCursor(GetCursor, OCR_NORMAL);
   OpenFile.Free;
 end;


end;

procedure TMorrowindWorker.RenameFile(Idx: integer; NewName: string);
begin
  inherited;

end;

procedure TMorrowindWorker.SaveFilesToArchive(ArchiveName: string);
begin
  inherited;

end;

end.
