unit libraryListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, applicationconfig,TreeListView, libraryParser;

type

{ TLibraryListView }

TLibraryListView = class(TTreeListView)
  lastCollapsed: TTreeListItem;
  procedure LibraryListViewClick(sender: TObject);
  procedure LibraryListViewCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
    var defaultDraw: Boolean);
  procedure LibraryListViewItemCollapsed(sender: TObject; item: TTreeListItem);
public
  constructor create(aowner: TComponent);
  function selectedLibrary: TLibrary;
  procedure DoSelect(item: TTreeListItem); override;
end;

implementation
uses bbutils, strutils, Graphics;

resourcestring
  rsCustom = 'selbst definierte';

{ TLibraryListView }

procedure TLibraryListView.LibraryListViewItemCollapsed(sender: TObject; item: TTreeListItem);
begin
  lastCollapsed := item;
end;

procedure TLibraryListView.LibraryListViewClick(sender: TObject);
begin
  if (Selected <> nil) then begin
    if (Selected.data.obj = nil) and (Selected.SubItems.Count>0) and (Selected <> lastCollapsed) then begin
      Selected.Expand;
      Selected := Selected.SubItems[0];
      if (selected.Parent.SubItems.Count = 1) and (Selected.data.obj = nil) then begin
        Selected.Expand;
        Selected := Selected.SubItems[0];
      end;
    end;
    lastCollapsed := nil;
  end;
end;

procedure TLibraryListView.LibraryListViewCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp;
  recordItem: TTreeListRecordItem; var defaultDraw: Boolean);
var
  lib: TLibrary;
begin
  if recordItem.Parent.data.p = nil then exit;
  lib := TLibrary(recordItem.Parent.data.p);
  if lib.tableComment = '' then exit;
  if recordItem.Index <> 0 then exit;
  canvas.Font.Color := clGray;
  canvas.Font.Style := [fsItalic];
  drawTextRect(lib.tableComment, canvas.TextWidth(recordItem.Text) + 15 * recordItem.Parent.Indent + 20, taLeftJustify, F_DrawingRecordItemRect, false);
  recordItem.selectFont(canvas);
end;

constructor TLibraryListView.create(aowner: TComponent);
var
  j: Integer;
  state, loc: String;
  libs: TStringArray;
begin
  inherited;
  BeginUpdate;
  for state in libraryManager.enumerateCountryStates() do begin
    with Items.Add(IfThen(state = '- - -', rsCustom, state)) do begin
      for loc in libraryManager.enumerateLocations(state) do
        if trim(loc) <> '' then
           with SubItems.Add(IfThen(loc = '-', rsCustom, loc)) do begin
             libs := libraryManager.enumeratePrettyLongNames(loc);
             for j := 0 to high(libs) do
               SubItems.Add(libs[j]).data.p:=libraryManager.getLibraryFromEnumeration(loc, j);
             Collapse;
           end;
      Collapse;
    end;
  end;
  HeaderVisible:=false;
  OnItemCollapsed:=@LibraryListViewItemCollapsed;
  OnClick:=@LibraryListViewClick;
  EndUpdate;
  RowHeight:=RowHeight+5;
  {$ifdef android}
  RowHeight:=RowHeight*2-10;
  {$endif}
  OnCustomRecordItemDraw:=@LibraryListViewCustomRecordItemDraw;
end;

function TLibraryListView.selectedLibrary: TLibrary;
var
  item: TTreeListItem;
begin
  item := Selected;
  if (item = nil) or (item.data.obj = nil) then exit(nil);
  result := item.data.obj as TLibrary;
end;

procedure TLibraryListView.DoSelect(item: TTreeListItem);
begin
  inherited DoSelect(item);

end;

end.

