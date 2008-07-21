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
    ComboBox1: TComboBox;
    Label1: TLabel;
    mausInfo: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
  private
    { private declarations }
    procedure translateDate(sender: TAxis; value: extended; var translated: string);
  public
    { public declarations }
    diagram: TDiagram;
    firstYear: longint;
    procedure updateStatistic;
  end;

const
  DIAGRAM_DAYS=0;
  DIAGRAM_WEEKS=1;
  DIAGRAM_MONTHS=2;
var
  statistikForm: TstatistikForm;

implementation

uses bookwatchmain,bbutils,booklistreader,libraryParser;
{ TstatistikForm }


procedure TstatistikForm.translateDate(sender: TAxis; value: extended; var translated: string);
begin
  case combobox1.itemindex of
    DIAGRAM_WEEKS: translated:= inttostr(weekOfYear(round(value)));
    //2: translated:= FormatDateTime('m',value);
    DIAGRAM_MONTHS: //translated:= format('%.2d-%.2d',[firstYear+(firstMonth+value) div 12, 1+(firstMonth+value-1) mod 12]);
       translated:= format('%.2d',[1+round(value) mod 12]);
    else translated:= FormatDateTime('d.m',round(value));
  end;

end;

procedure TstatistikForm.updateStatistic;
var i,j,k,c,tc,currentLoop:integer;
    checkDate,nextCheckDate: longint;
    y,m,d,y2,m2,d2,y3,m3,d3: word; //year,..
    books: TBookLists;
    book:TBook;
    earliestDay,lastDay: integer;
    
    showSum: boolean;


    accountValues,totalValues: array of longint;
    

begin
  showSum:=CheckBox1.Checked;

  diagram.clear;
  //Get limits (first known day) and add datalists
  earliestDay:=lastCheck;
  lastDay:=lastCheck;
  for i:=0 to accountIDs.Count-1 do begin
    with TCustomAccountAccess(accountIDs.Objects[i]) do begin
      for j:=0 to books.current.Count-1 do begin
        if (books.current[j].issueDate<>0) and (books.current[j].issueDate<earliestDay) then
          earliestDay:=books.current[j].issueDate;
        if (books.current[j].firstExistsDate<>0) and (books.current[j].firstExistsDate<earliestDay) then
          earliestDay:=books.current[j].firstExistsDate;
      end;
      for j:=0 to books.old.Count-1 do begin
        if (books.old[j].issueDate<>0) and (books.old[j].issueDate<earliestDay) then
          earliestDay:=books.old[j].issueDate;
        if (books.old[j].firstExistsDate<>0) and (books.old[j].firstExistsDate<earliestDay) then
          earliestDay:=books.old[j].firstExistsDate;
      end;

      diagram.addDataList.title:=prettyName;
    end;
  end;
  
  if showSum then diagram.addDataList.title:='Summe';


  case ComboBox1.itemIndex of
    DIAGRAM_WEEKS:  begin
      earliestDay:=earliestDay-earliestDay mod 7;
      lastDay:=lastDay+7-lastDay mod 7;
      SetLength(accountValues,(lastDay-earliestDay) div 7+1);
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
          if checkDate = 0 then continue;
          while checkdate<=book.lastExistsDate div 7 do begin
            accountValues[checkdate-earliestDay div 7]+=1;
            totalValues[checkdate-earliestDay div 7]+=1;
            checkdate+=1;
          end;
        end;
        for i:=0 to high(accountValues) do
          TDataList(diagram.dataLists[j]).addPoint(i*7+earliestDay,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          TDataList(diagram.dataLists[diagram.DataLists.Count-1]).addPoint(i*7+earliestDay,totalValues[i]);
    end;
    DIAGRAM_MONTHS:  begin
      DecodeDate(earliestDay,y,m,d);
      DecodeDate(lastDay,y2,m2,d2);
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
          if checkDate = 0 then continue;
          DecodeDate(checkDate,y2,m2,d2);
          DecodeDate(book.lastExistsDate,y3,m3,d3);
          for i:=y2*12+m2-1 - (y*12+m-1) to y3*12+m3-1  - (y*12+m-1) do begin
            accountValues[i]+=1;
            totalValues[i]+=1;
            checkdate+=1;
          end;
        end;
        for i:=0 to high(accountValues) do
          TDataList(diagram.dataLists[j]).addPoint(i+y*12+m-1,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          TDataList(diagram.dataLists[diagram.DataLists.Count-1]).addPoint(i+y*12+m-1,totalValues[i]);

    end;
    else begin//iterate about days
      SetLength(accountValues,lastDay-earliestDay+1);
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
          if checkDate = 0 then continue;
          while checkdate<=book.lastExistsDate do begin
            accountValues[checkdate-earliestDay]+=1;
            totalValues[checkdate-earliestDay]+=1;
            checkdate+=1;
          end;
        end;
        for i:=0 to high(accountValues) do
          TDataList(diagram.dataLists[j]).addPoint(i+earliestDay,accountValues[i]);
      end;
      if showSum then
        for i:=0 to high(totalValues) do
          TDataList(diagram.dataLists[diagram.DataLists.Count-1]).addPoint(i+earliestDay,totalValues[i]);
    end;
  end;
  diagram.update;
  PaintBox1Paint(PaintBox1);
end;

procedure TstatistikForm.FormCreate(Sender: TObject);
begin
  diagram:=TDiagram.create;
  diagram.XAxis.valueTranslate:=@translateDate;
  diagram.Diagram.Width:=400;
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

procedure TstatistikForm.ComboBox1Select(Sender: TObject);
begin
  updateStatistic;
end;

procedure TstatistikForm.FormDestroy(Sender: TObject);
begin
  diagram.free;
end;

procedure TstatistikForm.FormResize(Sender: TObject);
begin

end;

procedure TstatistikForm.FormShow(Sender: TObject);
begin
  updateStatistic;
end;

procedure TstatistikForm.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var day,year,month:longint;
begin
  case ComboBox1.ItemIndex of
    DIAGRAM_DAYS: begin
      day:=round(diagram.posXToDataX(x));
      mausInfo.Caption:=DateToStr(day);
    end;
    DIAGRAM_WEEKS: begin
      day:=round(diagram.posXToDataX(x));
      mausInfo.Caption:=DateToStr(day-DayOfWeek(day))+' - '+DateToStr(day+7-DayOfWeek(day));
    end;
    DIAGRAM_MONTHS:  begin
      month:=round(diagram.posXToDataX(x));
      year:=month div 12;
      month:=month mod 12+1;
      mausInfo.Caption:=DateToStr(EncodeDate(word(year),word(month),1))+' - '+DateToStr(IncMonth(EncodeDate(word(year),word(month),1))-1);;
    end;
  end;
end;

procedure TstatistikForm.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0,0,diagram.Diagram);
end;

procedure TstatistikForm.PaintBox1Resize(Sender: TObject);
begin
  if diagram=nil then exit;
  diagram.Diagram.SetSize(PaintBox1.Width,PaintBox1.Height);
  updateStatistic;
end;

initialization
  {$I statistik_u.lrs}

end.

