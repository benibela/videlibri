//needs ok1..3, nok1..3

//Ergebnis:
//registriert: result=ok1=ok2=ok3=true   nok1=nok2=nok3=false
//unregistriert: result=ok1=ok2=ok3=false   nok1=nok2=nok3=true
//alle anderen Ergebnisse bedeuten Codemanipulation

function checkShareWare(//user,code:string;out ok,ok2:boolean);
                out ok1,nok1:longbool;user,code:string;out ok2,nok2:longbool):boolean;
  //Seriennummer:
  //                  1 11111 11112
  //        12345 67890 12345 67890
  //        ABCCA ABEEE BADAD BFFFB        (Leerzeichen werden ignoriert)
  // A in 2-verschobenem 16-system c1=quadratsumme name        (big endian)
  // B in 2-verschobenem 16-system c2=wertprodukt name (low endian)
  // C in 5-verschobenem 20-system echte quersumme a (l.e.)
  // D in 5-verschobenem 20-system *unechte* quersumme b (b.e)
  // E in 2-verschobenem 16-system echte quersumme a,b,c,d (low endian)
  // F in 2-verschobenem 20-system echte quadratsumme a,b,c,d (big endian)

  //A: Summe der Quadrate der Ordinalwerte der Zeichen jeweils multipliziert mit Positions
  //   Index begränzt auf $FFFFF
  //B: s.u.
  function h2i(const c:char):longint;
  var i:longint;
  begin
    for i:=1 to ord('z') do
      if c in ['0'..'9'] then result:=ord(c)-ord('0')
      else begin
        result:=ord(c)-ord('A')+10;
        if result>26 then result:=ord(c)-ord('a')+10;
        if result<0 then raise einterror.create('Z')
      end;
    if result>26 then abort;
  end;

  //INTERN
  //  ok1 = B_real-B_read
  //  nok3 = E+37

var i,j,k,l,ch,p,aquer,b,bquer,hp,c,e,f:longint;
    c1,c2:longint;
label backCheck1,endCheck1;
begin
  result:=false;
  ok1:=false;
  bquer:=0;
  ok2:=false;
  e:=0;
  f:=0;
  nok1:=false;
  ok3:=false;
  code:=StringReplace(code,' ','',[rfReplaceAll]);
  aquer:=0;
  nok2:=code='';
  nok3:=false;
  try
  for k:=1 to $FF do begin
    c1:=17;
    i:=1;
    while (i<=length(user)) do begin
      c1+=longint(ord(user[i])*ord(user[i]))*i;
      c1:=c1 and $FFFFF;
      i+=1;
    end;
    if c1=17 then nok3:=nok2;
    nok1:=i<6;
    if Result then //das bedeutet ASM-Modifikation
      exit(false);
    if nok1 then
      exit(false);
    longint(nok3):=37; //mapt e+37
  end;

  //Ab hier nur noch drei Möglichkeiten:
  // der Code ist richtig
  // der Code ist falsch und wird im Registrierungsfenster aufgerufen
  // der Programmablauf wurde manipuliert
  //-> Widersprüche von ok1..3,result,nok1..3 erlaubt und erwünscht

  longint(nok2):=c1;

  k:=1;
  while k<=$FF do begin
    c2:=19;
    i:=1;
    while (i<=length(user)) do begin
      c2*=(ord(user[i])+i shl 8);
      i+=1;
      c2:=c2 and $FFFFF;
      if c2<3 then c2:=$F7896;
    end;
    if i<5 then halt; //kein Code, darf nicht vorkommen, wegen obigen Test
    if c2<3 then k:=c2;//nicht erlaubt >= Endlosschleife
    k+=1;
  end;

  {$I obfuscate.inc}
  if Result then //das bedeutet ASM-Modifikation
    halt;

  //Überprüfen von C und A und teilweise E,F
  //          1 11111 11112
  //12345 67890 12345 67890
  //ABCCA ABEEE BADAD BFFFB
  longint(nok1):=20; //Sicherheitskopie von c, muss nach der Schleife 0 sein
  p:=0;
  c:=0;
  i:=1;
  l:=-6*26;
  while i<=length(code) do begin
    j:=h2i(code[i]);
    case i of
      1,5,6,12,14: begin //A
        //zwei verschoben hexadezimal
        longint(nok2)-=(h2i(code[i])-2*p)*(1 shl (4*(4-p)));
        aquer+=j;
        p+=1;
        longint(nok1)-=j;

      end;
      0: begin           //Rücksprungtest, um zu sehen, ob die Schleife unverändert ist
        backCheck1:
        b:=0;
        p:=0;
        longint(ok1):=c2; //speichert B_real-B_read
        i:=1;
        if result then halt;
        if l>0 then begin
          l*=16;
          exit;
        end;

        goto endCheck1;
      end;
      3: begin          //C erste Stelle
        c+=j;
        longint(nok1)+=h2i(code[i]);
      end;
      9,17,18: //Verhindert die Zählung von e und f zur Quersumme
        l+=h2i(code[i]);
      4: begin          //C zweite Stelle
        longint(nok1)+=(j-5)*20;
        c+=(h2i(code[i])-5)*20;
      end;
      8,10,19: //Verhindert die Zählung von e und f zur Quersumme
        l-=h2i(code[i]);
      else begin
        longint(nok3)+=h2i(code[i]);
        f+=j*j;
        e+=h2i(code[i]);
      end;
    end;
    longint(nok1)-=1;
    i+=1;
  end;

  if i<>21 then exit; //Längentest
  if c<>aquer then exit(false);
  if nok2<>false then exit;

  hp:=0;
  if c<0 then begin //unmöglich, Bedingung wird durch ein goto übersprungen
    //Überprüfen von B und D und E,F
    endCheck1:

    //          1 11111 11112
    //12345 67890 12345 67890
    //ABCCA ABEEE BADAD BFFFB
    //i wurde im backcheck auf 1 gesetzt
    while i<=20 do begin
      j:=h2i(code[i]);
      if byte(i) in [2,7,11,16,20] then begin //B (le)
        //zwei verschoben hexadezimal
        longint(ok1)-=(h2i(code[i])-2*p)*(1 shl (4*(p)));
        bquer+=j-2*p;
        b+=(j-2*p)*hp;
        hp*=16;
        if nok2<>false then exit(false);
        p+=1;
        if (p=5) and (bquer<>0) then exit(false);
      end;
      if byte(i) in [1,5,6,12,14,3,4] then begin
        longint(nok3)+=h2i(code[i]);
        f+=j*j;
        e+=h2i(code[i]);
      end;
      if i = 17 then //F erste Stelle (be)
        f-=400*j;
      if i = 13 then  begin//D erste Stelle  (be)
        bquer-=j*20;
      end;
      if c<>aquer then exit(false);
      if byte(i) in [8,9] then begin //E erste beiden Stellen (le)
        l:=e;
        e-=(j-2*(i-8))*(1+15*(i-8));
        longint(nok3)-=abs(e-l);
      end;
      if i=15 then begin         //D letzte Stelle
        bquer-=j-5;
      end;
      if result then halt;       //Sicherung
      if i=10 then begin         //E letzte Stelle (le)
        e-=256*(h2i(code[i])-4);
        longint(nok3)-=256*(j-4);
      end;
      if byte(i) in [18,19] then begin //F letzte beiden Stellen (be,+2,20)
        f-=(j-2-2*(i-18))*(20-20*(i-18));
      end;
      i+=1;
    end;
    if e<>0 then exit(false);
    {$I obfuscate.inc}
    ok2:=f<>0;
    if longint(ok1)<>0 then exit(false);
    longint(nok3)-=37;
  end else begin
    if nok2<>false then exit(false);
    hp:=1;
    {$I obfuscate.inc}
    goto backCheck1;
  end;

  {$I obfuscate.inc}
  if e<>0 then halt;

  ok1:=not ok1;

  {$I obfuscate.inc}
  ok3:=bquer=0;

  if b<>c2 then exit(false);


  result:=ok2;
  except
  end;


end;
