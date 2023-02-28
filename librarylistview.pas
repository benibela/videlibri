unit libraryListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, applicationconfig,TreeListView, libraryParser;

type

{ TLibraryListView }

TLibraryListView = class(TTreeListView)
  lastCollapsed: TTreeListItem;
  procedure LibraryListViewClick({%H-}sender: TObject);
  procedure LibraryListViewCustomRecordItemDraw({%H-}sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
    var {%H-}defaultDraw: Boolean);
  procedure LibraryListViewItemCollapsed({%H-}sender: TObject; item: TTreeListItem);
public
  constructor create(aowner: TComponent);
  function selectedLibrary: TLibrary;
  procedure DoSelect(item: TTreeListItem); override;
end;

implementation
uses bbutils, strutils, Graphics;


{ TLibraryListView }

procedure TLibraryListView.LibraryListViewItemCollapsed(sender: TObject; item: TTreeListItem);
begin
  lastCollapsed := item;
end;

procedure lazyLoad(item: TTreeListItem);
begin
  if (item.SubItems.Count = 0) and (item.data.obj = nil) and (item.RecordItems.Count > 1) then begin
    item.data.obj := libraryManager.get(item.RecordItemsText[1]);
    if item.data.obj <> nil then item.Text := (item.data.obj as TLibrary).prettyName;
  end;
end;

procedure TLibraryListView.LibraryListViewClick(sender: TObject);
var
  i: Integer;
begin
  if (Selected <> nil) then begin
    BeginUpdate;
    if (Selected.data.obj = nil) and (Selected.SubItems.Count>0) and (Selected <> lastCollapsed) then begin
      Selected.Expand;
      for i := 0 to Selected.SubItems.Count - 1 do
        lazyLoad(selected.SubItems[i]);
      Selected := Selected.SubItems[0];
      if (selected.Parent.SubItems.Count = 1) and (Selected.SubItems.Count > 0) and (Selected.data.obj = nil) then begin
        Selected.Expand;
        for i := 0 to Selected.SubItems.Count - 1 do
          lazyLoad(selected.SubItems[i]);
        Selected := Selected.SubItems[0];
      end;
    end;
    EndUpdate;
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
  enumerator: TLibraryMetaDataEnumerator;
  currentCountryStateItem, currentLocationItem: TTreeListItem;
begin
  inherited;
  BeginUpdate;
  currentCountryStateItem := nil;
  currentLocationItem := nil;
  enumerator := libraryManager.enumerateLibraryMetaData;
  while enumerator.MoveNext do with enumerator do begin
    if enumerator.newCountryState then begin
       if currentCountryStateItem <> nil then currentCountryStateItem.Collapse;
       currentCountryStateItem := Items.Add(IfThen(prettyCountryState = '- - -', rsCustomLibrary, prettyCountryState));
    end;
    if enumerator.newLocation then begin
       if currentLocationItem <> nil then currentLocationItem.Collapse;
       currentLocationItem := currentCountryStateItem.SubItems.Add(IfThen(prettyLocation = '-', rsCustomLibrary, prettyLocation));
    end;
    currentLocationItem.SubItems.Add('').RecordItemsText[1] := libraryId;
  end;
  if currentCountryStateItem <> nil then currentCountryStateItem.Collapse;
  if currentLocationItem <> nil then currentLocationItem.Collapse;

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

