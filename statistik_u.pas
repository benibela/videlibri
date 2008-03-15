unit statistik_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,diagram,applicationconfig,libraryparser,
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
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
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
    firstYear, firstMonth: longint;
    procedure updateStatistic;
  end;

const
  DIAGRAM_DAYS=0;
  DIAGRAM_WEEKS=1;
  DIAGRAM_MONTHS=2;
var
  statistikForm: TstatistikForm;

implementation

uses bookwatchmain,bbutils;
{ TstatistikForm }


procedure TstatistikForm.translateDate(sender: TAxis; value: extended; var translated: string);
begin
  case combobox1.itemindex of
    1: translated:= inttostr(weekOfYear(round(value)));
    //2: translated:= FormatDateTime('m',value);
    2: //translated:= format('%.2d-%.2d',[firstYear+(firstMonth+value) div 12, 1+(firstMonth+value-1) mod 12]);
       translated:= format('%.2d',[1+(firstMonth+round(value)-1) mod 12]);
    else translated:= FormatDateTime('d.m',round(value));
  end;

end;

procedure TstatistikForm.updateStatistic;
var i,j,k,c,tc,currentLoop:integer;
    checkDate,nextCheckDate: longint;
    y,m,d: word; //year,..
    books: tbooklists;
    book:PBook;
    earliestDay,lastDay: integer;
    
    showSum: boolean;
begin
  showSum:=CheckBox1.Checked;

  diagram.clear;
  //Get limits (first known day) and add datalists
  earliestDay:=lastCheck;
  lastDay:=lastCheck;
  for i:=0 to accountIDs.Count-1 do begin
    for j:=0 to TCustomAccountAccess(accountIDs.Objects[i]).getBooks().getBookCount(botAll)-1 do
      if (TCustomAccountAccess(accountIDs.Objects[i]).getBooks().getBook(botAll,j)^.issueDate<>0)and
         (TCustomAccountAccess(accountIDs.Objects[i]).getBooks().getBook(botAll,j)^.issueDate<earliestDay) then
        earliestDay:=TCustomAccountAccess(accountIDs.Objects[i]).getBooks().getBook(botAll,j)^.issueDate;
    diagram.addDataList.title:=TCustomAccountAccess(accountIDs.Objects[i]).prettyName;
  end;
  
  if showSum then diagram.addDataList.title:='Summe';
  

  case ComboBox1.itemIndex of
    DIAGRAM_WEEKS:  begin
          earliestDay:=earliestDay-DayOfWeek(earliestDay);
          checkDate:=earliestDay;
          repeat
            tc:=0;
            for j:=0 to accountIDs.Count-1 do begin
              c:=0;
              books:=TCustomAccountAccess(accountIDs.Objects[j]).getBooks();
              for k:=0 to books.getBookCount(botAll)-1 do begin
                book:=books.getBook(botAll,k);
                if (book^.issueDate<=checkDate+6)and(book^.lastExistsDate>=checkDate) then
                  inc(c);
              end;
              TDataList(diagram.dataLists[j]).addPoint(checkDate,c);
              inc(tc,c);
            end;
            if showSum then TDataList(diagram.DataLists[diagram.DataLists.Count-1]).addPoint(checkDate,tc);
            inc(checkDate,7);
          until checkDate>=lastDay;
        end;
    DIAGRAM_MONTHS:  begin
      DecodeDate(earliestDay,y,m,d);
      nextCheckDate:=longint(trunc(EncodeDate(y,m,1)));
      checkDate:=nextCheckDate;
      currentLoop:=0;
      firstYear:=y;
      firstMonth:=m;
      repeat
        inc(m);
        if m>12 then begin
          inc(y);
          m:=1;
        end;
        nextCheckDate:=longint(trunc(EncodeDate(y,m,1)));
        tc:=0;
        for j:=0 to accountIDs.Count-1 do begin
          c:=0;
          books:=TCustomAccountAccess(accountIDs.Objects[j]).getBooks();
          for k:=0 to books.getBookCount(botAll)-1 do begin
            book:=books.getBook(botAll,k);
            if (book^.issueDate<nextCheckDate)and(book^.lastExistsDate>=checkDate) then inc(c);
          end;
          TDataList(diagram.dataLists[j]).addPoint({checkDate}currentLoop,c);
          inc(tc,c);
        end;
        if showSum then
          TDataList(diagram.DataLists[diagram.DataLists.Count-1]).addPoint({checkDate}currentLoop,tc);
        checkDate:=nextCheckDate;
        inc(currentLoop);
      until checkDate>=lastDay;
    end;
    else //iterate about days
      for i:=earliestDay to lastCheck do begin
        tc:=0;
        for j:=0 to accountIDs.Count-1 do begin
          c:=0;
          books:=TCustomAccountAccess(accountIDs.Objects[j]).getBooks();
          for k:=0 to books.getBookCount(botAll)-1 do begin
            book:=books.getBook(botAll,k);
            if (book^.issueDate<=i)and(book^.lastExistsDate>=i) then inc(c);
          end;
          TDataList(diagram.dataLists[j]).addPoint(i,c);
          inc(tc,c);
        end;
        if showSum then TDataList(diagram.DataLists[diagram.DataLists.Count-1]).addPoint(i,tc);
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

procedure TstatistikForm.ComboBox1Change(Sender: TObject);
begin

end;

procedure TstatistikForm.ComboBox1Click(Sender: TObject);
begin
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
      year:=firstYear+month div 12;
      month:=(firstMonth+month-1) mod 12+1;
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
  diagram.Diagram.Width:=PaintBox1.Width;
  diagram.Diagram.Height:=PaintBox1.Height;
  updateStatistic;
end;

initialization
  {$I statistik_u.lrs}

end.

