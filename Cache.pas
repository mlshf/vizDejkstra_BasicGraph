unit Cache;

interface

uses maps, dbconn, dbaTools, MapObject, Classes, SysUtils;

type

  TEdge = class
  private
    FNodeIn : Integer;
    FNodeOut : Integer;
    FMOS : TMapObjectStructure;
  public
    property NodeIn : Integer read FNodeIn write FNodeIn;
    property NodeOut : Integer read FNodeOut write FNodeOut;
    property MOS : TMapObjectStructure read FMOS write FMOS;
  end;

  TEdge2 = class
  private
    FNodeOut : Integer;
    FLength : Integer;
  public
    property NodeOut : Integer read FNodeOut write FNodeOut;
    property Length : Integer read FLength write FLength;
  end;

  TArrayInteger = Array of Integer;
  TArrayOfArrayEdge = Array of Array of TEdge2;

  TCache = class
  private
    FMapObjects : TMapObjects;

  public
    property MapObjects : TMapObjects read FMapObjects write FMapObjects;
    procedure init (Conn : TDbaConnection);
    procedure GetEdgesByNodeIn(NodeIn : Integer; var ResultArray : TArrayInteger; var ResultAmount : Integer);
    procedure GetEdgesByNodeOut(NodeOut : Integer; var ResultArray : TArrayInteger; var ResultAmount : Integer);
  end;

  TVertexCache = class
  private
    FNeighbors : TArrayOfArrayEdge;
  public
    property Neighbors : TArrayOfArrayEdge read FNeighbors write FNeighbors;
    procedure init (Conn : TDbaConnection);

  end;





implementation

{**********************************************************************************************
* init - заполнить кэш данными (дугами)
***********************************************************************************************}
procedure TCache.init(Conn : TDbaConnection);
var
  sql : string;
  dbResult : TDBResult;
  Edge : TEdge;
  line : PChar;
  MOS : TMapObjectStructure;
begin
  sql := 'SELECT okey, NodeIn, NodeOut, Line FROM uds.graph_sections gs WHERE gs.sign_deleted = 0';
  Conn.QueryOpen(sql, dbResult, true);

  While dbResult.Fetch() do
  begin
    Edge := TEdge.Create();
    Edge.NodeIn := dbResult.asInteger(1);
    Edge.NodeOut := dbResult.asInteger(2);
    MOS := TMapObjectStructure.Create();
    dbResult.asBlob(3,line);
    MOS.GetFromBuf( PByte(line), -1, 0, 0, 0, 1 );
    Edge.MOS := MOS;
    FMapObjects.addItem(dbResult.asString(0), Edge);
  end;
  FreeAndNil(dbResult);
end;

{**********************************************************************************************
* init - заполнить кэш данными (смежными вершинами)
***********************************************************************************************}
procedure TVertexCache.init(Conn : TDbaConnection);
var
  sql : string;
  dbResult : TDBResult;
  i, size : Integer;
  line : PChar;
  MOS : TMapObjectStructure;
begin
  sql := 'SELECT MAX(okey) FROM uds.graph_nodes';
  Conn.QueryOpen(sql, dbResult, true);
  dbResult.Fetch();
  size := dbResult.asInteger(0) + 1;
  SetLength(FNeighbors, size);
  MOS := TMapObjectStructure.Create();
  {sql := 'SELECT okey FROM uds.graph_nodes';
  Conn.QueryOpen(sql, dbResult, true);
  while dbResult.Fetch() do
  begin
    sql := 'SELECT NodeOut, LINE FROM uds.graph_sections WHERE NodeIn = ' + dbResult.asString(0);
    Conn.QueryOpen(sql, dbTempResult, true);
    If (dbTempResult.numRows) > 0 Then
    begin
      SetLength(FNeighbors[dbResult.asInteger(0)], dbTempResult.numRows);
      i := 0;
      While dbTempResult.Fetch() do
      begin
        FNeighbors[dbResult.asInteger(0), i] := TEdge2.Create();
        FNeighbors[dbResult.asInteger(0), i].NodeOut := dbTempResult.asInteger(0);
        dbTempResult.asBlob(1,line);
        MOS.GetFromBuf( PByte(line), -1, 0, 0, 0, 1 );
        FNeighbors[dbResult.asInteger(0), i].Length := Round(MOS.GetLength);
        Inc(i);
      end;
    end;
    FreeAndNil(dbTempResult);
  end;
  FreeAndNil(dbResult);
  FreeAndNil(MOS);}

  sql := 'SELECT gn.OKEY, gs.NodeOut, gs.LINE FROM uds.graph_nodes gn INNER JOIN uds.graph_sections gs ON gn.OKEY = gs.NodeIn';
  Conn.QueryOpen(sql, dbResult, true);
  while dbResult.Fetch() do
  begin
    i := Length(FNeighbors[dbResult.asInteger(0)]);
    SetLength(FNeighbors[dbResult.asInteger(0)], i + 1);

    FNeighbors[dbResult.asInteger(0), i] := TEdge2.Create();
    FNeighbors[dbResult.asInteger(0), i].NodeOut := dbResult.asInteger(1);
    dbResult.asBlob(2,line);
    MOS.GetFromBuf( PByte(line), -1, 0, 0, 0, 1 );
    FNeighbors[dbResult.asInteger(0), i].Length := Round(MOS.GetLength);
  end;
  FreeAndNil(dbResult);
  FreeAndNil(MOS);

end;

{**********************************************************************************************
* GetNodeInByNodeIn - получить список индексов дуг в FMapObjects дуг по NodeIn
***********************************************************************************************}
procedure TCache.GetEdgesByNodeIn(NodeIn : Integer; var ResultArray : TArrayInteger; var ResultAmount : Integer);
var
  i : integer;
begin
  ResultAmount := 0;
  SetLength(ResultArray, 0);

  For i := 0 to FMapObjects.Count - 1 do
  begin

    If TEdge(FMapObjects.items[i]).NodeIn = NodeIn Then
    begin
      Inc(ResultAmount);
      SetLength(ResultArray, ResultAmount);
      ResultArray[ResultAmount - 1] := i;

    end;
  end;

end;

{**********************************************************************************************
* GetNodeInByNodeOut - получить список индексов дуг в FMapObjects дуг по NodeOut
***********************************************************************************************}
procedure TCache.GetEdgesByNodeOut(NodeOut : Integer; var ResultArray : TArrayInteger; var ResultAmount : Integer);
var
  i : integer;
begin
  ResultAmount := 0;
  SetLength(ResultArray, 0);
  For i := 0 to FMapObjects.Count - 1 do
  begin
    If TEdge(FMapObjects.items[i]).NodeOut = NodeOut Then
    begin
      Inc(ResultAmount);
      SetLength(ResultArray, ResultAmount);
      ResultArray[ResultAmount - 1] := i;
    end;
  end;
end;

end.
