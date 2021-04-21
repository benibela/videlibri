unit vlmaps;

{$mode objfpc}{$H+}

interface

uses
  simplehtmltreeparser,xquery,xquery.internals.common;

type
  //generic THashmapStrOwningGenericObject<TValue> = class(specialize TXQHashmapStrOwningGenericObject<TValue>);
  //THashmapStrOwningObject = xquery.internals.common.TXQHashmapStrOwningObject;
  TMapStringOwningObject = xquery.TXQMapStringOwningObject;
  //TMapStringObject = TXQMapStringObject;
  TSetOfString = TXQHashsetStr;

implementation

end.

