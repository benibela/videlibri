unit statistik_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,diagram,applicationconfig,
  StdCtrls, Buttons;

type

  { TstatistikForm }

  TstatistikForm = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    mausInfo: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure translateDate(sender: TAxis; value: extended; var translated: string);
  public
    { public declarations }
    diagramDrawer: TDiagramDrawer;
    diagramModel: TDiagramDataListModel;
    firstYear: longint;
    procedure updateStatistic;
  end;

const
  DIAGRAM_DAYS=0;
  DIAGRAM_WEEKS=1;
  DIAGRAM_MONTHS=2;
  DIAGRAM_YEARS=3;
var
  statistikForm: TstatistikForm;

implementation

uses bookwatchmain,bbutils,math,booklistreader,libraryParser;
{ TstatistikForm }


procedure TstatistikForm.translateDate(sender: TAxis; value: extended; var translated: string);
begin
  case combobox1.itemindex of
    DIAGRAM_WEEKS: translated:= inttostr(dateWeekOfYear(round(value)));
    //2: translated:= dateTimeFormat('m',value);
    DIAGRAM_MONTHS: //translated:= format('%.2d-%.2d',[firstYear+(firstMonth+value) div 12, 1+(firstMonth+value-1) mod 12]);
       translated:= format('%.2d',[1+round(value) mod 12]);
    DIAGRAM_YEARS:
       translated:= IntToStr(round(value));
    else translated:= dateTimeFormat('d.m',round(value));
  end;
end;

procedure TstatistikForm.updateStatistic;
var i,j,k:integer;
    checkDate: longint;
    y,m,d,y2,m2,d2,y3,m3,d3: word; //year,..
    books: TBookLists;
    book:TBook;
    earliestDay,lastDay: integer;
    
    showSum: boolean;


    accountValues,totalValues: array of longint;
    onlyFirstDay: boolean;

    

begin
  if diagramDrawer=nil then exit;

  showSum:=CheckBox1.Checked;

  diagramModel.deleteLists;
  diagramModel.Flags:=[mfEditable];
  //Get limits (first known day) and add datalists
  earliestDay:=lastCheck;
  lastDay:=lastCheck;
  for i:=0 to accountIDs.Count-1 do begin
    with TCustomAccountAccess(accountIDs.Objects[i]) do begin
      for j:=0 to books.current.Count-1 do begin
        if (books.current[j].issueDate>0) and (books.current[j].issueDate<earliestDay) then
          earliestDay:=books.current[j].issueDate;
        if (books.current[j].firstExistsDate>0) and (books.current[j].firstExistsDate<earliestDay) then
          earliestDay:=books.current[j].firstExistsDate;
      end;
      for j:=0 to books.old.Count-1 do begin
        if (books.old[j].issueDate>0) and (books.old[j].issueDate<earliestDay) then
          earliestDay:=books.old[j].issueDate;
        if (books.old[j].firstExistsDate>0) and (books.old[j].firstExistsDate<earliestDay) then
          earliestDay:=books.old[j].firstExistsDate;
      end;

      diagramModel.addDataList.Title:=prettyName;
    end;
  end;
  
  if showSum then diagramModel.addDataList.Title:='Summe';

 // exit;

  onlyFirstDay := ComboBox2.ItemIndex = 1;

  case ComboBox1.itemIndex of
    DIAGRAM_WEEKS:  begin
      earliestDay:=earliestDay-earliestDay mod 7;
      lastDay:=lastDay+7-lastDay mod 7;
      SetLength(accountValues,(max(lastDay,currentDate)-earliestDay) div 7+1);
      SetLength(totalValues,length(accountValues));
      FillChar(totalValues[0],length(totalValues)*sizeof(totalValues[0]),0);
      for j:=0 to accountIDs.Count-1 do begin
        FillChar(accountValues[0],length(accountValues)*sizeof(accountValues[0]),0);
        books:=TCustomAccountAccess(accountIDs.Objects[j]).books;
        for k:=-books.old.count to books.current.count-1 do begin //loop about union
          if k<0 then book:=books.old[-k-1]
          else book:=books.current[k];
          checkdate:=book.issueDate div 7;
          if checkDate = 0 then checkDate:=book.firstExistsDate div 7;
          if checkDate <= 0 then continue;
          while (checkdate<=book.lastExistsDate div 7) do begin
            accountValues[checkdate-earliestDay div 7]+=1;
            totalValues[checkdate-earliestDay div 7]+=1;
            checkdate+=1;
            if onlyFirstDay then break;
          end;
        end;
        for i:=0 to high(accountValues) do
          diagramModel.addData(j,i*7+earliestDay,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          diagramModel.addData(diagramModel.dataRows-1,i*7+earliestDay,totalValues[i]);
    end;
    DIAGRAM_MONTHS:  begin
      DecodeDate(earliestDay,y,m,d);
      DecodeDate(max(lastDay,currentDate),y2,m2,d2);
      SetLength(accountValues,y2*12+m2-1  - (y*12+m-1) + 1);
      SetLength(totalValues,length(accountValues));
      FillChar(totalValues[0],length(totalValues)*sizeof(totalValues[0]),0);
      for j:=0 to accountIDs.Count-1 do begin
        FillChar(accountValues[0],length(accountValues)*sizeof(accountValues[0]),0);
        books:=TCustomAccountAccess(accountIDs.Objects[j]).books;
        for k:=-books.old.count to books.current.count-1 do begin //loop about union
          if k<0 then book:=books.old[-k-1]
          else book:=books.current[k];
          checkdate:=book.issueDate;
          if checkDate = 0 then checkDate:=book.firstExistsDate;
          if checkDate <= 0 then continue;
          DecodeDate(checkDate,y2,m2,d2);
          DecodeDate(book.lastExistsDate,y3,m3,d3);
          for i:=y2*12+m2-1 - (y*12+m-1) to y3*12+m3-1  - (y*12+m-1) do begin
            accountValues[i]+=1;
            totalValues[i]+=1;
            checkdate+=1;
            if onlyFirstDay then break;
          end;
        end;
        for i:=0 to high(accountValues) do
          diagramModel.addData(j,i+y*12+m-1,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          diagramModel.addData(diagramModel.dataRows-1,i+y*12+m-1,totalValues[i]);

    end;
    DIAGRAM_YEARS:  begin
      DecodeDate(earliestDay,y,m,d);
      DecodeDate(max(lastDay,currentDate),y2,m2,d2);
      SetLength(accountValues,y2 - y + 1);
      SetLength(totalValues,length(accountValues));
      FillChar(totalValues[0],length(totalValues)*sizeof(totalValues[0]),0);
      for j:=0 to accountIDs.Count-1 do begin
        FillChar(accountValues[0],length(accountValues)*sizeof(accountValues[0]),0);
        books:=TCustomAccountAccess(accountIDs.Objects[j]).books;
        for k:=-books.old.count to books.current.count-1 do begin //loop about union
          if k<0 then book:=books.old[-k-1]
          else book:=books.current[k];
          checkdate:=book.issueDate;
          if checkDate = 0 then checkDate:=book.firstExistsDate;
          if checkDate <= 0 then continue;
          DecodeDate(checkDate,y2,m2,d2);
          DecodeDate(book.lastExistsDate,y3,m3,d3);
          for i:=y2 to y3 do begin
            accountValues[i-y]+=1;
            totalValues[i-y]+=1;
            if onlyFirstDay then break;
          end;
        end;
        for i:=0 to high(accountValues) do
          diagramModel.addData(j,i+y,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          diagramModel.addData(diagramModel.dataRows-1,i+y,totalValues[i]);

    end;
    else begin//iterate about days
      SetLength(accountValues,max(lastDay,currentDate)-earliestDay+1);
      SetLength(totalValues,length(accountValues));
      FillChar(totalValues[0],length(totalValues)*sizeof(totalValues[0]),0);
      for j:=0 to accountIDs.Count-1 do begin
        FillChar(accountValues[0],length(accountValues)*sizeof(accountValues[0]),0);
        books:=(accountIDs.Objects[j] as TCustomAccountAccess).books;
        for k:=-books.old.count to books.current.count-1 do begin //loop about union
          if k<0 then book:=books.old[-k-1]
          else book:=books.current[k];
          checkdate:=book.issueDate;
          if checkDate = 0 then checkDate:=book.firstExistsDate;
          if checkDate <= 0 then continue;
          while checkdate<=book.lastExistsDate do begin
            accountValues[checkdate-earliestDay]+=1;
            totalValues[checkdate-earliestDay]+=1;
            checkdate+=1;
          end;
        end;
        for i:=0 to high(accountValues) do
          diagramModel.addData(j,i+earliestDay,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          diagramModel.addData(diagramModel.dataRows-1,i+earliestDay,totalValues[i]);
    end;
  end;
  diagramDrawer.AutoSetRangeY:=false;
  diagramDrawer.RangeMinY:=0;
  diagramDrawer.RangeMaxY:=diagramModel.maxY;
  diagramDrawer.update();
  PaintBox1.Update;
end;

procedure TstatistikForm.FormCreate(Sender: TObject);
begin
  diagramDrawer:=nil;
  ComboBox1.ItemIndex:=0;
end;

procedure TstatistikForm.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TstatistikForm.CheckBox1Click(Sender: TObject);
begin
  updateStatistic;
end;

procedure TstatistikForm.CheckBox2Change(Sender: TObject);
begin
  if diagramDrawer=nil then exit;
  if CheckBox2.Checked then diagramDrawer.FillStyle:=fsMinOverMax
  else diagramDrawer.FillStyle:=fsNone;
  diagramDrawer.update;
  PaintBox1.Update;
end;

procedure TstatistikForm.ComboBox1Select(Sender: TObject);
begin
  updateStatistic;
end;

procedure TstatistikForm.FormDestroy(Sender: TObject);
begin
  if diagramDrawer=nil then exit;
  diagramDrawer.Free;
end;

procedure TstatistikForm.FormResize(Sender: TObject);
begin

end;

procedure TstatistikForm.FormShow(Sender: TObject);
begin
  //updateStatistic;
end;

procedure TstatistikForm.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var day,year,month:longint;
begin
  if diagramDrawer=nil then exit;
  case ComboBox1.ItemIndex of
    DIAGRAM_DAYS: begin
      day:=round(diagramDrawer.posToDataX(x));
      mausInfo.Caption:=DateToSimpleStr(day);
    end;
    DIAGRAM_WEEKS: begin
      day:=round(diagramDrawer.posToDataX(x));
      mausInfo.Caption:=DateToSimpleStr(day-DayOfWeek(day))+' - '+DateToSimpleStr(day+7-DayOfWeek(day));
    end;
    DIAGRAM_MONTHS:  begin
      month:=round(diagramDrawer.posToDataX(x));
      year:=month div 12;
      month:=month mod 12+1;
      mausInfo.Caption:=DateToSimpleStr(EncodeDate(word(year),word(month),1))+' - '+DateToSimpleStr(IncMonth(EncodeDate(word(year),word(month),1))-1);;
    end;
    DIAGRAM_YEARS:  begin
      year:=round(diagramDrawer.posToDataX(x));
      mausInfo.Caption:=IntToStr(year);
    end;
  end;
  mausInfo.Caption := mausInfo.Caption + ': '+inttostr(round(diagramDrawer.posToDataY(y)));
end;

procedure TstatistikForm.PaintBox1Paint(Sender: TObject);
begin
  if diagramDrawer=nil then exit;
  PaintBox1.Canvas.Draw(0,0,diagramDrawer.Diagram);
end;

procedure TstatistikForm.PaintBox1Resize(Sender: TObject);
begin
  if diagramDrawer=nil then exit;
  diagramDrawer.Diagram.SetSize(PaintBox1.Width,PaintBox1.Height);
  updateStatistic;
end;

procedure TstatistikForm.Timer1Timer(Sender: TObject);
begin
  //workaround for LCL bug 18524
  timer1.Enabled:=false;
  if diagramDrawer<>nil then exit;
  diagramDrawer:=TDiagramDrawer.create;
  diagramModel:=TDiagramDataListModel.create;
  diagramDrawer.SetModel(diagramModel,true);
  diagramDrawer.BottomAxis.valueTranslate:=@translateDate;
  diagramDrawer.Diagram.SetSize(PaintBox1.Width,PaintBox1.Height);
  updateStatistic;
end;

initialization
  {$I statistik_u.lrs}

end.
