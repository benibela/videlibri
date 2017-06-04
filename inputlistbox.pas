{
  © Benito van der Zander 2017

  Modified from the Lazarus Component Library (LCL)

  Same license (LGPL + linking exception) applies
}

unit inputlistbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function InputList(const ACaption, APrompt: string; const AList: TStrings; var ASelected : Integer) : String;
function InputList(const ACaption, APrompt: string; const AList: TStrings) : Integer;


implementation

uses forms, StdCtrls, dialogs, ButtonPanel, math, controls;

function InputList(const ACaption, APrompt: string; const AList: TStrings; var ASelected : Integer) : String;
const minwidth = 200;
  Margin=24;
  Sep=8;

var
  W,I: Integer;
  Frm: TForm;
  list : Tlistbox;
  LPrompt: TLabel;
  BP: TButtonPanel;
begin
  Result:='';
  Frm:=TForm.Create(Application);
  try
    // Determine needed width
    W:=frm.Canvas.TextWidth(APrompt);
    W:=Max(W,frm.Canvas.TextWidth(ACaption));
    for I:=0 to AList.Count-1 do
      W:=Max(W,frm.Canvas.TextWidth(AList[i]+'WWW')); // WWW is just some extra.
    w := max(w, minwidth);
    frm.BorderStyle:=bsSizeable;
    frm.Caption:=ACaption;
    frm.ClientWidth:=W+2*Margin;
    frm.Position:=poScreenCenter;
    // Prompt
    LPrompt:=TLabel.Create(frm);
    LPrompt.Parent:=frm;
    LPrompt.Caption:=APrompt;
    LPrompt.SetBounds(Margin,Margin,Frm.ClientWidth-2*Margin,frm.Canvas.TextHeight(APrompt));
    LPrompt.WordWrap:=True;
    LPrompt.AutoSize:=False;
    // Selection listbox
    list := TListBox.Create(Frm);
    list.Parent:=Frm;
    list.Items.Assign(AList);
    list.ItemIndex:=ASelected;
    list.Left:=Margin;
    list.Top:=LPrompt.Top + LPrompt.Height + Sep;
    list.Width:=Frm.ClientWidth-2*Margin;
    // Buttons
    BP:=TButtonPanel.Create(Frm);
    BP.Parent:=Frm;
    BP.ShowButtons:=[pbOK,pbCancel];

    list.Height := min(max(list.ItemHeight, frm.Canvas.TextHeight('µHW^,.,;') + 7) * max(4, AList.Count),
                       max(screen.Height, 480) - bp.Height - 3*Sep - 3*Margin
                       ) ;
    Frm.ClientHeight:=LPrompt.Height+list.Height+BP.Height+2*Sep+Margin;
    LPrompt.Anchors := [akTop, akLeft, akRight];
    list.Anchors := [akTop, akLeft, akRight, akBottom];
    if (Frm.ShowModal=mrOk) then
    begin
      if (list.ItemIndex >= 0) and (list.ItemIndex < AList.Count) then
        Result:=AList[list.ItemIndex];
      ASelected:=list.ItemIndex;
    end else ASelected := -1;
  finally
    FreeAndNil(Frm);
  end;
end;

function InputList(const ACaption, APrompt: string; const AList: TStrings): Integer;
begin
  result := 0; //do not default to no selection
  InputList(ACaption, APrompt, AList, result);
end;

end.

