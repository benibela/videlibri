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
  procedure LibraryListViewItemCollapsed(sender: TObject; item: TTreeListItem);
public
  constructor create(aowner: TComponent);
  function selectedLibrary: TLibrary;
  procedure DoSelect(item: TTreeListItem); override;
end;

implementation
uses bbutils;

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
    end;
    lastCollapsed := nil;
  end;
end;

constructor TLibraryListView.create(aowner: TComponent);
var
  locs: TStringArray;
  libs: TStringArray;
  i, j: Integer;
begin
  inherited;
  BeginUpdate;
  locs := libraryManager.enumerateLocations;
  for i := 0 to high(locs) do
    if trim(locs[i]) <> '' then
      with Items.Add(trim(locs[i])) do begin
        libs := libraryManager.enumeratePrettyLongNames(locs[i]);
        for j := 0 to high(libs) do
          SubItems.Add(libs[j]).data.p:=libraryManager.getLibraryFromEnumeration(locs[i], j);
        Collapse;
      end;
  HeaderVisible:=false;
  OnItemCollapsed:=@LibraryListViewItemCollapsed;
  OnClick:=@LibraryListViewClick;
  EndUpdate;
  RowHeight:=RowHeight+5;
  {$ifdef android}
  RowHeight:=RowHeight*2-10;
  {$endif}
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

