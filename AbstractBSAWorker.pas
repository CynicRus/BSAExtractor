unit AbstractBSAWorker;
interface
uses
    System.Classes,System.SysUtils,System.Generics.Collections;
  const
   ErrIncorrectBSAVersion = 'Incorrect version of BSA file!';
   ErrExtractionFile = 'Extraction file error!';
   ErrNotSelectedFile = 'We dont have the selected file!';
   ErrPathNotSelected = 'Path not selected!';
  type
   TBSAType = (bsaMorrowind = 0,bsaOblivion = 1,bsaSkyrim = 2);
   TUpdateFunction = procedure (ArchName: string;FilesCount: integer) of object;
   TProcessFileFunction = procedure (Processed,Total: integer) of object;
   TUpdateListFunction = procedure (Filename: string;Offset,Size,Index: integer) of object;
   TSetMaxProgressFunction = procedure (MaxProgress: integer) of object;
   TCurrentProgressFunction = procedure (Progress: integer; Text: string) of object;
   TZeroProgressFunction = procedure () of object;
   TAbstractFile = class
   private
      FName: string;
      FDirectory: string;
      FSize: integer;
      FOffset: Integer;
      FLoadFlag: Boolean;
      FHash1: Int64;
      FHash2: Int64;
    public
      Constructor Create;
      procedure Reset;virtual;abstract;
      property Name: string read Fname write FName;
      property Size: Integer read FSize write FSize;
      property Offset: integer read FOffset write FOffset;
      property Directory: string read FDirectory write FDirectory;
      property LoadFlag: Boolean read FLoadFlag write FLoadFlag;
      property Hash1: int64 read FHash1 write FHash1;
      property Hash2: int64 read FHash2 write FHash2;
  end;

  TAbstractFileList = class
  private
    FAbstractFiles: TList;
    function GetCount: Integer;
    function GetAbstractFile(Index: Integer): TAbstractFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Src: TAbstractFileList);
    procedure Add(aAbstractFile: TAbstractFile); overload;
    procedure Add(aAbstractFiles: TAbstractFileList); overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(aAbstractFile: TAbstractFile); overload;
    function IndexOf(aAbstractFile: TAbstractFile): Integer;

    property Count: Integer read GetCount;
    property AbstractFile[Index: Integer]: TAbstractFile read GetAbstractFile; default;
  end;

   TAbstractBSAWorker = class
     private
      FUpdateFunction: TUpdateFunction;
      FProcessFileFunction: TProcessFileFunction;
      FUpdateListFunction: TUpdateListFunction;
      FSetMaxProgressFunction: TSetMaxProgressFunction;
      FCurrentProgressFunction: TCurrentProgressFunction;
      FZeroProgressFunction: TZeroProgressFunction;
      FArchiveName: string;
      FExtractionPath: string;
     public
      Constructor Create;
      Destructor Destroy;override;
      procedure OpenArchive(ArchiveName: string);virtual;abstract;
      function ExtractFile(Idx: integer): boolean;virtual;abstract;
      function ExtractDirectory(const DirectoryName: string): Boolean;virtual;abstract;
      function ExtractAllFiles: boolean;virtual;abstract;
      procedure DrawFilesByDirectory(const DirectoryName: string);virtual;abstract;
      procedure RenameFile(Idx: integer;NewName: string);virtual;abstract;
      procedure DeleteFile(Idx: integer);virtual;abstract;
      procedure InsertFileToArchive(Idx: integer);virtual;abstract;
      procedure SaveFilesToArchive(ArciveName: string);virtual;abstract;
      property UpdateFunction: TUpdateFunction read FUpdateFunction write FUpdateFunction;
      property ProcessFileFunction: TProcessFileFunction read FprocessFileFunction write FProcessFileFunction;
      property UpdateListFunction: TUpdateListFunction read FUpdateListFunction write FUpdateListFunction;
      property SetMaxProgressFunction: TSetMaxProgressFunction read FSetMaxProgressFunction write FSetMaxProgressFunction;
      property CurrentProgressFunction: TCurrentProgressFunction read FCurrentProgressFunction write FCurrentProgressFunction;
      property ZeroProgressFunction: TZeroProgressFunction read FZeroProgressFunction write FZeroProgressFunction;
      property ArchiveName: string read FArchiveName write FArchiveName;
      property ExtractionPath: string read FExtractionPath write FExtractionPath;
   end;


implementation
 const
   ErrItemNotFound = 'Item not found!';

{ TWTExtractor }

constructor TAbstractBSAWorker.Create;
begin

end;

destructor TAbstractBSAWorker.Destroy;
begin

  inherited;
end;

{ TAbstractFile }

constructor TAbstractFile.Create;
begin

end;

constructor TAbstractFileList.Create;
begin
  FAbstractFiles := TList.Create;
end;

destructor TAbstractFileList.Destroy;
begin
  Clear;
  FAbstractFiles.Free;
  inherited;
end;

procedure TAbstractFileList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create(ErrItemNotFound);

  AbstractFile[Index].Free;
  FAbstractFiles.Delete(Index);
end;

procedure TAbstractFileList.Delete(aAbstractFile: TAbstractFile);
begin
  Delete(IndexOf(aAbstractFile));
end;

procedure TAbstractFileList.Add(aAbstractFiles: TAbstractFileList);
var
  I: Integer;
begin
  for I := 0 to aAbstractFiles.Count - 1 do
    Add(aAbstractFiles[I]);
end;

procedure TAbstractFileList.Add(aAbstractFile: TAbstractFile);
begin
  FAbstractFiles.Add(aAbstractFile);
end;

procedure TAbstractFileList.Assign(Src: TAbstractFileList);
begin
  Clear;
  Add(Src);
end;

procedure TAbstractFileList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AbstractFile[I].Free;
  FAbstractFiles.Clear;
end;


function TAbstractFileList.GetCount: Integer;
begin
  Result := FAbstractFiles.Count;
end;

function TAbstractFileList.GetAbstractFile(Index: Integer): TAbstractFile;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TAbstractFile(FAbstractFiles[Index])
  else
    Result := nil;
end;

function TAbstractFileList.IndexOf(aAbstractFile: TAbstractFile): Integer;
begin
  Result := FAbstractFiles.IndexOf(aAbstractFile);
end;

end.
