unit libraryListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, applicationconfig, TreeListView, libraryParser;

type

{ TLibraryListView }

TLibraryListView = class(TTreeListView)
  lastCollapsed: TTreeListItem;
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
  EndUpdate;
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
  if (item.data.obj = nil) and (item.SubItems.Count>0) and (item <> lastCollapsed) then begin
    item.Expand;
    Selected := item.SubItems[0];
  end;
end;

end.

