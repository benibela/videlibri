unit bookrowpicu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,math,LCLIntf,process;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private

  public
    pic: TBitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
//Wandelt eine HLS-Farbe in RGB um.
//h = farbton, s = sättigung, l = helligkeit,
//Die Funktionsweise habe herausgefunden, in dem ich mir im Malprogramm angesehen
//habe, was sich ändert wenn der Farbton bei voller Sättigung und 50% Helligkeit
//erhöht/verringert wird, und anschließend was bei Änderungen der Sättigung
//und Helligkeit jeweils passiert, und dann entsprechend den 3-Satz angewandt habe.
//Hintereinander ausgeführt hat es dann sehr gut zusammen gepasst.
procedure HLSToRGB(const h,s,l:integer;out r_out,g_out,b_out:byte);
var r,g,b:integer;
begin
  //Grundfarbton berechen, die reinen Grundfarbtönen sind Farbverläufe zwischen
  //den Werten die jeweils bei Vielfachen von 60° auftreten:
  if h<=60 then begin                          //0° : rot
    r:=255;
    g:=h*255 div 60;
    b:=0;
  end else if h<=120 then begin                //60° : gelb
    r:=255-(h-60)*255 div 60;
    g:=255;
    b:=0;
  end else if h<=180 then begin                //120° : grün
    r:=0;
    g:=255;
    b:=(h-120)*255 div 60;
  end  else if h<=240 then begin               //180° : cyan
    r:=0;
    g:=255-(h-180)*255 div 60;
    b:=255;
  end  else if h<=300 then begin               //240° : blau
    r:=(h-240)*255 div 60;
    g:=0;
    b:=255;
  end  else if h<=360 then begin               //300° : violett
    r:=255;
    g:=0;
    b:=255-(h-300)*255 div 60;
  end else begin //Das sollte nie eintreten
    r:=0;
    g:=0;
    b:=0;
  end;
  //Sättigung, die Farbwerte nähert sich immer mehr 128
  if s< 100 then begin
    r:=(r-128)*s div 100 + 128;
    g:=(g-128)*s div 100 + 128;
    b:=(b-128)*s div 100 + 128;
  end;
  //Helligkeit
  if l<50 then begin //Dunkler (die Farbwerte nähern sich immer mehr 0)
    r:=r * l div 50;
    g:=g * l div 50;
    b:=b * l div 50;
  end else if l>50 then begin //Heller (die Farbwerte nähern sich immer mehr 255)
    r:=(255-r) * (l - 50) div 50 + r;
    g:=(255-g) * (l - 50) div 50 + g;
    b:=(255-b) * (l - 50) div 50 + b;
  end;
  r_out:=byte(min(max(r,0),255));
  g_out:=byte(min(max(g,0),255));
  b_out:=byte(min(max(b,0),255));
end;

//Wandelt eine HLS-Farbe in eine TColor-Farbe um
function HLSToRGBColor(const h,s,l:integer):TColor;
var r,g,b:byte;
begin
  HLSToRGB(h,s,l,r,g,b);
  result:=rgb(r,g,b);
end;
//Wandelt RGB in HLS um.
//Den Algorithmus habe ich gefunden, nachdem mir aufgefallen war, dass der
//höchste und niedrigste RGB-Farbwert vor der Berechnung der Sättigung und
//Helligkeit 255 und 0 war, so dass ich ein Gleichungssystem zurück rechnen konnte.
procedure RGBToHLS(const r,g,b:integer;out h,s,l:integer);
var colors:array[0..2] of byte;
    i:integer;
    lowest, biggest,middest:integer;
    s1,s2,l1,l2:single;
    d1,d2:single;
    tmid:single;
begin
  if (r=g)and (g=b) then begin
    //grau
    h:=0;
    l:=g*100 div 255;
    s:=0;
    exit;
  end;
  colors[0]:=r;
  colors[1]:=g;
  colors[2]:=b;
  lowest:=0;
  biggest:=0;
  //Höchsten und niedrigsten Wert finden
  for i:=1 to 2 do begin
    if colors[i]>colors[biggest] then biggest:=i;
    if colors[i]<colors[lowest] then lowest:=i;
  end;
  //Farbwert in der Mitte finden
  middest:=0;
  for i:=1 to 2 do
    if (i<>lowest) and (i<>biggest) then middest:=i;
  {for i:=1 to 2 do
    if (colors[i]<>colors[biggest]) and  (colors[i]<>colors[lowest]) then
      middest:=i;}
  //lowest war direkt nach der Farbtonberechnung bei HLSToRGB exakt 0
  //biggest war direkt nach der Farbtonberechnung bei HLSToRGB exakt 255
  //middest dazwischen
  //Lösung des Gleichungssystem das bei "dunkler", durch einsetzten der Ausgabe
  //von der Sättigungsberechnung in die der Helligketisberechnung, entsteht
  s1:=(12800*(colors[biggest] - colors[lowest])) / (127*colors[lowest] + 128*colors[biggest]);
  l1:= 5*(127*colors[lowest] + 128*colors[biggest])/3264;
  //Lösung des Gleichungssystem das bei "heller" entsteht
  s2:= (12700*(colors[lowest] - colors[biggest]))/ (127*colors[lowest] + 128*colors[biggest] - 65025);
  l2:= 10*(127*colors[lowest] + 128*colors[biggest] - 255)/6477;

  if (l1<0) or (l1>55) or (s1<0) or (s1>100) then begin
    //Erste Lösung ungültig (wenn die Helligkeit über 50 ist, müsste die Lösung
    //für das Gleichungssystem für "heller" genommen werden
    s:=round(s2);
    l:=round(l2);
  end else if (s2<0) or (s2>100)or (s2<0) or (s2>100) then begin
    //Erste Lösung ungültig (wenn die Helligkeit über 50 ist, müsste die Lösung
    //für das Gleichungssystem für "dunkler" genommen werden
    s:=round(s1);
    l:=round(l1);
  end else begin
    //Beide Lösungen sind gültig, deshalb werden die Farbwerte berechnet,
    //die herauskommen, wenn die Werte eingesetz werden.
    //Erstes Paar
    if l1<50 then
      d1:=abs(((-128)*s1 / 100 + 128) * l1 / 50 -  colors[lowest]) +
          abs(((255-128)*s1 / 100 + 128) * l1 / 50 -  colors[biggest])
     else
      d1:=abs((l1*(32*s1 + 3175) - 50*(64*s1 - 25))/1250 -  colors[lowest]) +
          abs((100*(127*s1 + 50) - 127*l1*(s1 - 100))/5000 -  colors[biggest]);
    //Zweites Paar
    if l2<50 then
      d2:=abs(((-128)*s2 / 100 + 128) * l2 / 50 -  colors[lowest]) +
          abs(((255-128)*s2 / 100 + 128) * l2 / 50 -  colors[biggest])
     else
      d2:=abs((l2*(32*s2 + 3175) - 50*(64*s2 - 25))/1250 -  colors[lowest]) +
          abs((100*(127*s2 + 50) - 127*l2*(s2 - 100))/5000 -  colors[biggest]);
    //Die Lösung mit den kleinsten Unterschieden zu den echten Werten wird genommt
    if d1>d2 then begin
      s:=round(s2);
      l:=round(l2);
    end else begin
      s:=round(s1);
      l:=round(l1);
    end;
  end;
  //Ist die Helligkeit maximale/minimal oder die Sättigung minimal kann der Farb-
  //ton nicht berechnet werden.
  if (l=100)or(l=0)or(s=0) then begin
    h:=0;
    exit;
  end;

  //Jetzt ist die Helligkeit und die Sättigung bekannt, und der Farbton kann aus
  //middest zurückgerechnet werden
  //Helligkeit zurück
  if l<=50 then tmid:=colors[middest] * 50 / l  //Dunkler
  else tmid:=(colors[middest]-255 * (l - 50) / 50)/(1-(l - 50) / 50); //Heller

  //Sättigung zurück
  tmid:=(tmid-128)*100/s+128;

  //Farbton zurück
  case biggest of
    0: case lowest of //tR = 255
         1: h:=round((255-tmid)*60/255+300); //h zwischen 300 und 360
         2: h:=round(tmid*60/255); //h zwischen 0 und 60
       end;
    1: case lowest of //tG = 255
         0: h:=round(tmid*60/255 + 120);     //h zwischen 120 und 180
         2: h:=round((255-tmid)*60/255+60); //h zwischen 60 und 120
       end;
    2: case lowest of //tB = 255
         0: h:=round((255-tmid)*60/255+180);//h zwischen 180 und 240
         1: h:=round(tmid*60/255 + 240);//h zwischen 240 und 300
       end;
  end;
end;



procedure TForm1.Image1Click(Sender: TObject);
begin

end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin

end;

function randbetween(a,b:integer): integer;
begin
  result := random(b-a)+a;
end;

type TMyColorBuffer = array of array of tcolor;

function loadPNG(fn: string): TMyColorBuffer;
var
  back: TPortableNetworkGraphic;
  backbmp: TBitmap;
  y, x: Integer;
  line: Pointer;
begin
  back := TPortableNetworkGraphic.Create;
  back.LoadFromFile(fn);

  //we need to get the pixels, but back.canvas.pixels only returns black
  //scanline returns weird artifacts, and changing pixelformat after loading has no effect

  backbmp := TBitmap.Create;
  backbmp.PixelFormat:=pf32bit;
  backbmp.Width := back.width;
  backbmp.Height := back.Height;
  backbmp.Canvas.Draw(0,0,back);
  backbmp.beginupdate;
  setlength(result, backbmp.Width,backbmp.Height);
  for y:=0 to backbmp.Height-1 do begin
    line := backbmp.ScanLine[y];
    for x:=0 to backbmp.Width-1 do
      result[x,y] := pcardinal(line)[x];
  end;
  backbmp.endupdate;
  backbmp.free;
  back.free;
end;

procedure rgbsplit(c: tcolor; out r,g,b: integer);
begin
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
end;

function rgbblend(c1, c2: TColor; scale: integer{0..255}): tcolor;
var
  r1,r2,g1,g2,b1,b2, scale2: integer;
begin
  rgbsplit(c1,r1,g1,b1);
  rgbsplit(c2,r2,g2,b2);
  scale2 := 255 - scale;
  result := RGBToColor(
    r1 * scale div 255 + r2 * scale2 div 255,
    g1 * scale div 255 + g2 * scale2 div 255,
    b1 * scale div 255 + b2 * scale2 div 255);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  bx, by, y, x, ry, rx: Integer;
  repcolor: TColor;
  backbuffer,resbuffer, pagebuffer: TMyColorBuffer;
  line: Pointer;

 function atback(x,y: integer): tcolor;
 begin
   if (x < 0) or (x > high(backbuffer)) or (y < 0) or (y > high(backbuffer[0])) then
     result := clWhite
    else
     result := backbuffer[x,y];
 end;

const backwidth = 19;
      shelfheight = 25;

var backgroundcolorbottom, backgroundcolortop: tcolor;
  yshelfbottom: Integer;
begin
  backgroundcolortop := StrToInt('$'+edit1.Text);
  backgroundcolorbottom := StrToInt('$'+edit2.Text);

  backbuffer := loadPNG('bookback.png'); //back of the book
  pagebuffer := loadPNG('bookpages.png');
  pic := TBitmap.Create;
  pic.Width := 1024;
  pic.Height := 100;
  pic.PixelFormat:=pf32bit;

  setlength(resbuffer, pic.width,pic.height);
  yshelfbottom := high(resbuffer[0]) - shelfheight;
  for x :=0 to high(resbuffer) do begin
    for y :=0 to yshelfbottom - 1 do
      resbuffer[x,y]:=backgroundcolortop;
    for y :=yshelfbottom to high(resbuffer[x]) do
      resbuffer[x,y]:=backgroundcolorbottom;
  end;


  bx := pic.Width;
  by := pic.Height - high(backbuffer[0]) - 5;
  while bx > 0 do begin
    repcolor := HLSToRGBColor(random(360), randbetween(20,80), randbetween(30,70));
    //rescanv.Draw(bx,by,back);
    for ry:=0 to high(backbuffer[0]) do begin
      y := ry + by;
      if y > high(resbuffer[0]) then break;
      if y < 0 then continue;
      for rx:=0 to high(backbuffer) do begin
        x := rx + bx;
        if x > high(resbuffer) then break;
        if x < 0 then continue;
        case backbuffer[rx, ry] and $00FFFFFF of
          clAqua: resbuffer[x,y] := pagebuffer[x mod length(pagebuffer), y mod length(pagebuffer[0])];
          clRed: resbuffer[x,y] := repcolor;
          clBlue: begin
            if Random(1000) < 250 then begin
              resbuffer[x,y] := clBlack;
              if x + 1 < high(resbuffer) then begin
                if Random(1000) < 700 then begin
                  resbuffer[x+1,y+randbetween(-1,1)] := clBlack;
                end;
              end;
            end else resbuffer[x,y] := repcolor;
          end;
          clWhite: ;
          else if (backbuffer[rx, ry] and $FF = (backbuffer[rx, ry] shr 8) and $FF) and
                  (backbuffer[rx, ry] and $FF = (backbuffer[rx, ry] shr 16) and $FF) then begin
            if y <= yshelfbottom then
              resbuffer[x,y] := rgbblend(backgroundcolortop, backbuffer[rx,ry], backbuffer[rx, ry] and $FF)
             else
              resbuffer[x,y] := rgbblend(backgroundcolorbottom, backbuffer[rx,ry], backbuffer[rx, ry] and $FF)
          end else  resbuffer[x,y] := backbuffer[rx,ry];
        end;
      end;
  {  for i:=bx to bx + back.Width do
      pic.Canvas.MoveTo(bx+i, byy);
    end;}
    end;
    bx -= backwidth;
  end;
  {for y := yshelfbottom  to high(resbuffer[0]) do begin
    for x := 0 to high(resbuffer) do begin
      if resbuffer[x][y] and $00FFFFFF = clWhite then
        resbuffer[x][y] := backgroundcolorbottom;
    end;
  end;}
  pic.BeginUpdate();
  for y := 0 to pic.Height - 1 do begin
    line := pic.ScanLine[y];
    for x := 0 to pic.Width - 1 do
      pcardinal(line)[x] {AABBGGRR} := $FF000000 or cardinal(resbuffer[x,y]);
  end;
  pic.EndUpdate();

  pic.SaveToFile('/tmp/videlibritmp.bmp');
  pic.free;
  image1.Picture.LoadFromFile('/tmp/videlibritmp.bmp');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

end.

