unit vlmaps;

{$mode objfpc}{$H+}

interface

uses
  simplehtmltreeparser,xquery;

type
  generic THashmapStrOwningGenericObject<TValue> = class(specialize TXQHashmapStrOwningGenericObject<TValue>);
  THashmapStrOwningObject = simplehtmltreeparser.TXQHashmapStrOwningObject;
  TMapStringOwningObject = TXQMapStringOwningObject;
  //TMapStringObject = TXQMapStringObject;

implementation

end.

