unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Zlib, AbstractBSAWorker, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Menus;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    StatusBar1: TStatusBar;
    FileDlg: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    ExtractMenu: TPopupMenu;
    Extract1: TMenuItem;
    Extractall1: TMenuItem;
    TestTree: TTreeView;
    PopupExtract: TPopupMenu;
    ExtractDirectory1: TMenuItem;
    ExctractAll1: TMenuItem;
    procedure N2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure Extractall1Click(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ExtractDirectory1Click(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure WMDROPFILES(var msg: TWMDropFiles); message WM_DROPFILES;
    { Private declarations }
  public
    function CheckType(const Filename: string): TBSAType;
    Procedure UpdateStatusBar(ArchName: string; FilesCount: Integer);
    Procedure ProcessedFiles(Processed, Total: Integer);
    Procedure AddToListView(Filename: string; Offset, Size, Idx: Integer);
    procedure SetMaxProgress(MaxP: Integer);
    procedure ClearProgress();
    procedure NextStep(StepPos: Integer; Text: string);
    procedure FillTreeViewWithFiles(TreeView1: TTreeView;
      Strs: TAbstractFileList);
    { Public declarations }
  end;

var
  Form1: TForm1;
  Extractor: TAbstractBSAWorker = nil;
  ProgressBar: TProgressBar = nil;
  lbl: TLabel = nil;

  // CurrIndex: integer;
implementation

uses
  MorrowindBSA, OtherBSA, FileCtrl, ShellApi;

{$R *.dfm}

procedure TForm1.AddToListView(Filename: string; Offset, Size, Idx: Integer);

  function FormatByteSize(const bytes: Longint): string;
  const
    B = 1; // byte
    KB = 1024 * B; // kilobyte
    MB = 1024 * KB; // megabyte
    GB = 1024 * MB; // gigabyte
  begin
    if bytes > GB then
      result := FormatFloat('#.## GB', bytes / GB)
    else if bytes > MB then
      result := FormatFloat('#.## MB', bytes / MB)
    else if bytes > KB then
      result := FormatFloat('#.## KB', bytes / KB)
    else
      result := FormatFloat('#.## bytes', bytes);
  end;

var
  ListItem: TListItem;
begin
  ListItem := ListView1.Items.Add;
  ListView1.Items.BeginUpdate;
  With ListItem do
  begin
    Caption := Filename;
    Subitems.Add(FormatByteSize(Size));
    Subitems.Add(IntToHex(Offset, 8));
    Data := TObject(Idx);
  end;
  ListView1.Items.EndUpdate;

end;

function TForm1.CheckType(const Filename: string): TBSAType;
var
  FileReader: TFileStream;
  FieldID: Cardinal;
  ext: string;
  Ver: Integer;
begin
  ext := ExtractFileExt(Filename);
  Ver := 0;
  if CompareText(LowerCase(ext), '.bsa') > 0 then
    raise Exception.Create(ErrNotSupported);
  FieldID := 0;
  FileReader := TFileStream.Create(Filename, fmShareDenyRead);
  result := bsaUnknown;
  try
    FileReader.Read(FieldID, 4);
    if (FieldID = 256) then
      result := bsaMorrowind
    else if (FieldID = 4281154) then
    begin
      FileReader.Read(Ver, 4);
      if (Ver <> 103) and (Ver <> 104) then
        raise Exception.Create(ErrIncorrectBSAVersion);
      case Ver of
        103:
          result := bsaOblivion;
        104:
          result := bsaSkyrim;
      end;
    end
    else
      raise Exception.Create(ErrIncorrectBSAVersion);
  finally
    FileReader.Free;
  end;
end;

procedure TForm1.ClearProgress;
begin
  ProgressBar.Position := 0;
  lbl.Caption := '';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Destroy;
end;

procedure TForm1.Extract1Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('Select Directory', ExtractFileDrive(Dir), Dir,
    [sdNewUI, sdNewFolder]) then
    Extractor.ExtractionPath := Dir
  else
    raise Exception.Create(ErrPathNotSelected);

  if (ListView1.ItemIndex < 0) then
    raise Exception.Create(ErrNotSelectedFile);

  if Extractor.ExtractFile(Integer(ListView1.Items[ListView1.ItemIndex].Data))
  then
    ShowMessage('Successfully completed!')
  else
    raise Exception.Create(ErrExtractionFile);
end;

procedure TForm1.Extractall1Click(Sender: TObject);
var
  Dir: string;
begin
  if TestTree.Items.Count <= 0 then
    exit;
  if SelectDirectory('Select Directory', ExtractFileDrive(Dir), Dir,
    [sdNewUI, sdNewFolder]) then
    Extractor.ExtractionPath := Dir
  else
    raise Exception.Create(ErrPathNotSelected);

  if Extractor.ExtractAllFiles then
    ShowMessage(' Successfully completed!')
  else
    raise Exception.Create(ErrExtractionFile);
end;

procedure TForm1.ExtractDirectory1Click(Sender: TObject);
var
  TreeNode: TTreeNode;
  DirName, Dir: string;
begin
  DirName := '';
  if (TestTree.Selected = nil) then
    exit;
  TreeNode := TestTree.Selected;

  if TreeNode.Parent <> nil then
  begin
    while TreeNode <> nil do
    begin
      DirName := TreeNode.Text + '\' + DirName;
      TreeNode := TreeNode.Parent;
    end
  end
  else
    DirName := TreeNode.Text + '\';

  if SelectDirectory('Select Directory', ExtractFileDrive(Dir), Dir,
    [sdNewUI, sdNewFolder]) then
    Extractor.ExtractionPath := Dir
  else
    raise Exception.Create(ErrPathNotSelected);

  if Extractor.ExtractDirectory(DirName) then
    ShowMessage('Successfully completed!')
  else
    raise Exception.Create(ErrExtractionFile);

  // ShowMessage(TestTree.Selected.Text);
  // Extractor.
end;

procedure TForm1.FillTreeViewWithFiles(TreeView1: TTreeView;
  Strs: TAbstractFileList);
Var
  CachedStrs: TStringList;

  Procedure AddItem(Lev: Integer; ParentNode: TTreeNode; S: String);
    Function FindNodeWithText(AParent: TTreeNode; Const S: String): TTreeNode;
    Var
      K: Integer;
      fStr: String;
      tmpNode: TTreeNode;
    Begin
      result := Nil;
      fStr := S + IntToStr(Integer(AParent));
      K := CachedStrs.IndexOf(fStr);
      If K > -1 Then
        result := Pointer(CachedStrs.Objects[K])
      Else
      Begin
        If AParent <> Nil Then
          tmpNode := AParent.getFirstChild
        Else
          tmpNode := TreeView1.Items.GetFirstNode;
        While tmpNode <> Nil Do
        Begin
          If tmpNode.Text = S Then
          Begin
            result := tmpNode;
            CachedStrs.AddObject(fStr, Pointer(tmpNode));
            break;
          End;
          tmpNode := tmpNode.getNextSibling;
        End;
      End
    End;

  Var
    prefix: String;
    ID: Integer;
    aNode: TTreeNode;
  Begin
    If S = '' Then
      exit;
    ID := Pos('\', S);
    prefix := '';
    If ID > 0 Then
      prefix := Copy(S, 1, ID - 1)
    Else
    Begin
      prefix := S;
      S := '';
    End;

    aNode := FindNodeWithText(ParentNode, prefix);

    If aNode = Nil Then
    Begin
      aNode := TreeView1.Items.AddChild(ParentNode, prefix);
    End;

    AddItem(Lev + 1, aNode, Copy(S, ID + 1, Length(S)));

  End;

Var
  K: Integer;
Begin

  CachedStrs := TStringList.Create;
  CachedStrs.Duplicates := dupIgnore;
  CachedStrs.Sorted := True;

  Try

    TreeView1.Items.BeginUpdate;
    TreeView1.SortType := stNone;

    For K := 0 To Strs.Count - 1 Do
      AddItem(0, Nil, Strs[K].Directory);

  Finally
    TreeView1.Items.EndUpdate;
    CachedStrs.Free;
  End;
End;

function CreateProgressBar(StatusBar: TStatusBar; index: Integer): TProgressBar;
var
  FindLeft: Integer;
  i: Integer;
begin
  result := TProgressBar.Create(StatusBar);
  with result do
  begin
    Parent := StatusBar;
    visible := True;
    top := 2;
    FindLeft := 0;
    for i := 0 to index - 1 do
      FindLeft := FindLeft + StatusBar.Panels[i].width + 1;
    left := FindLeft + 160;
    width := StatusBar.Panels[index].width + 100;
    height := StatusBar.height - 2;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProgressBar := CreateProgressBar(StatusBar1, 2);
  ProgressBar.Min := 0;
  lbl := TLabel.Create(nil);
  lbl.Parent := ProgressBar;
  lbl.left := (ProgressBar.width - lbl.width) div 2;
  lbl.top := 2;
  lbl.Transparent := True;
  DragAcceptFiles(Self.Handle, True);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (Extractor <> nil) then
    Extractor.Free;
  if (lbl <> nil) then
    lbl.Free;
  if (ProgressBar <> nil) then
    ProgressBar.Free;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  i: Integer;
begin
  TestTree.Items.BeginUpdate;
  TestTree.Items.Clear;
  TestTree.Items.EndUpdate;

  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;
  ListView1.Items.EndUpdate;
  if (Extractor <> nil) then
    Extractor.Free;
  if FileDlg.Execute then
  begin
    case CheckType(FileDlg.Filename) of
      bsaMorrowind:
        Extractor := TMorrowindWorker.Create;
      bsaSkyrim, BsaFallout3, bsaOblivion:
        Extractor := TBSAWorker.Create;
      bsaUnknown:
        raise Exception.Create(ErrIncorrectBSAVersion);
    end;
    with Extractor do
    begin
      UpdateFunction := UpdateStatusBar;
      ProcessFileFunction := ProcessedFiles;
      UpdateListFunction := AddToListView;
      SetMaxProgressFunction := SetMaxProgress;
      ZeroProgressFunction := ClearProgress;
      CurrentProgressFunction := NextStep;
      OpenArchive(FileDlg.Filename);
      //SaveFilesToArchive(ExtractFilePath(FileDlg.Filename) + 'test.bsa');
    end;
    if (Extractor is TBSAWorker) then
      FillTreeViewWithFiles(TestTree, TBSAWorker(Extractor).Files)
    else
      FillTreeViewWithFiles(TestTree, TMorrowindWorker(Extractor).Files);

  end
  else
    exit;

end;

procedure TForm1.NextStep(StepPos: Integer; Text: string);
begin
  ProgressBar.Position := ProgressBar.Position + StepPos;
  lbl.Caption := Format('%d/%d', [ProgressBar.Position, ProgressBar.Max]);
  with StatusBar1.Panels do
  begin
    Items[2].Text := Text;
  end;

end;

procedure TForm1.ProcessedFiles(Processed, Total: Integer);
begin
  with StatusBar1.Panels do
  begin
    Items[4].Text := IntToStr(Processed) + '/' + IntToStr(Total);
  end;
end;

procedure TForm1.SetMaxProgress(MaxP: Integer);
begin
  ProgressBar.Max := MaxP;
end;

procedure TForm1.TestTreeChange(Sender: TObject; Node: TTreeNode);
var
  TreeNode: TTreeNode;
  DirName: string;
begin
  if Assigned(Node) then
  begin
    ListView1.Clear;
    TreeNode := Node;
    DirName := '';
    if TreeNode.Parent <> nil then
    begin
      while TreeNode <> nil do
      begin
        DirName := TreeNode.Text + '\' + DirName;
        TreeNode := TreeNode.Parent;
      end
    end
    else
      DirName := TreeNode.Text + '\';
    Extractor.DrawFilesByDirectory(DirName);
  end;
end;

procedure TForm1.TestTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    TestTree.Selected := TestTree.GetNodeAt(X, Y);

end;

procedure TForm1.UpdateStatusBar(ArchName: string; FilesCount: Integer);
begin
  with StatusBar1.Panels do
  begin
    Items[0].Text := ExtractFileName(ArchName);
    Items[1].Text := IntToStr(FilesCount);
  end;

end;

procedure TForm1.WMDROPFILES(var msg: TWMDropFiles);
const
  MAXFILENAME = 255;
var
  fileCount: Integer;
  Filename: array [0 .. MAXFILENAME] of char;
begin
  fileCount := DragQueryFile(msg.Drop, $FFFFFFFF, Filename, MAXFILENAME);

  if fileCount > 1 then
    raise Exception.Create(ErrCannotDragMoreThanOne);

  DragQueryFile(msg.Drop, 0, Filename, MAXFILENAME);

  if (Extractor <> nil) then
    Extractor.Free;

  TestTree.Items.BeginUpdate;
  TestTree.Items.Clear;
  TestTree.Items.EndUpdate;

  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;
  ListView1.Items.EndUpdate;

  case CheckType(Filename) of
    bsaMorrowind:
      Extractor := TMorrowindWorker.Create;
    BsaFallout3, bsaOblivion, bsaSkyrim:
      Extractor := TBSAWorker.Create;
    bsaUnknown:
      raise Exception.Create(ErrIncorrectBSAVersion);
  end;
  with Extractor do
  begin
    UpdateFunction := UpdateStatusBar;
    ProcessFileFunction := ProcessedFiles;
    UpdateListFunction := AddToListView;
    SetMaxProgressFunction := SetMaxProgress;
    ZeroProgressFunction := ClearProgress;
    CurrentProgressFunction := NextStep;
    OpenArchive(Filename);
  end;
  if (Extractor is TBSAWorker) then
    FillTreeViewWithFiles(TestTree, TBSAWorker(Extractor).Files)
  else
    FillTreeViewWithFiles(TestTree, TMorrowindWorker(Extractor).Files);
  DragFinish(msg.Drop);
end;

end.
