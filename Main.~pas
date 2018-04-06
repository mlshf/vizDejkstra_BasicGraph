unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls, ComCtrls,
  VirtualTrees,
  uniControl, uniSearchComboBox, uniStatusBar, uniMessageBox, uniBaseTypes,
  dbconn, dbaTools,
  maps, mapsSys,
  Mappl, MapplTypes, MapplMath, MapObject, MapObjectTypes, Animation,
  Map,
  Cache,
  Math,
  AliasSettings,
  uniTypes, Contnrs,
  DBACore, sqlLoader,
  testGraphForm, basicGraph;

const
  LAYER_CODE_BUILDING = 6;
  LAYER_CODE_UDS_ROAD = 7;
  LAYER_CODE_EDGES = 13;
  LAYER_CODE_VERTICES = 14;
  MAX_ROAD_WIDTH = 30;

  STREETS= 'asot.oas_streets';
  ROADS = 'egko.road';

type
  TFMain = class(TForm)
    status: TuniStatusBar;
    GBStart: TGroupBox;
    LStartStreet: TLabel;
    LStartHouse: TLabel;
    BSetStartAddress: TSpeedButton;
    CBStartHouse: TSearchComboBox;
    CBStartStreet: TSearchComboBox;
    GBEnd: TGroupBox;
    LEndStreet: TLabel;
    LEndHouse: TLabel;
    BSetEndAddress: TSpeedButton;
    BGetEndAddressFromMap: TSpeedButton;
    CBEndHouse: TSearchComboBox;
    CBEndStreet: TSearchComboBox;
    PToolbar: TPanel;
    BCalculate: TBitBtn;
    GBResult: TGroupBox;
    PResultRoutesType: TPanel;
    RBBranches: TRadioButton;
    RBStreets: TRadioButton;
    VTResult: TVirtualStringTree;
    BGetStartAddressFromMap: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure CBStartStreetChange(Sender: TObject);
    procedure CBEndStreetChange(Sender: TObject);
    procedure CBStartHouseChange(Sender: TObject);
    procedure CBEndHouseChange(Sender: TObject);
    procedure BGetStartAddressFromMapClick(Sender: TObject);
    procedure BGetEndAddressFromMapClick(Sender: TObject);
    procedure BCalculateClick(Sender: TObject);
    procedure BSetStartAddressClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BSetEndAddressClick(Sender: TObject);
    procedure VTResultGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure RBBranchesClick(Sender: TObject);
    procedure VTResultClick(Sender: TObject);
    procedure VTResultChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FMap : TFMap;
    FGraph: TBasicGraph;
    IniAliasOptions : TIniAliasOptions;
    mapStreets, mapStartAddresses, mapEndAddresses: TMapStrings;
    ConnUran3312, Conn: TDbaConnection;
    FStartBuildingMuid, FEndBuildingMuid: string;
    flDelStartPoint, flDelEndPoint : boolean;

    GroupId: integer;           // id группы кнопок
    StartNodeButtonId: integer; // id кнопки выбора начала пути
    EndNodeButtonId: integer;   // id кнопки выбора конца пути
    StartNodeMapMode: integer;  // MapMode выбора начала пути
    EndNodeMapMode: integer;    // MapMode выбора конца пути
    totalWayLength: integer;
    Cache : TCache;                  //Кэш с дугами
    VertexCache : TVertexCache;      //Кэш со смежными вершинами
    VertexValues : Array of Integer; //Массив минимального пути до вершин
    TempObjects  : TMapplTempObjects;
    ResultMosList : TMapObjectStructureList; //MOSlist с дугами;
  private

    procedure fillStreets();
    procedure fillStartAddresses(aStreetMUID: string);
    procedure fillEndAddresses(aStreetMUID: string);
    function GetStartOkey(var nodeMUID : Int64; conn: TDBAConnection) : boolean;
    function GetEndOkey(var nodeMUID : Int64; conn: TDBAConnection) : boolean;
    function HighlightTheSegmentOnly() : boolean;
    procedure HighlightTheWay(Conn: TDbaConnection; way : Array of Integer; wayAmount : Integer);

    procedure PrepareResultData (ParentNode: PVirtualNode);
    procedure FillVTResult();
    procedure filterResultTree(parentNode: PVirtualNode);
  public

    // обработчики смены режима Маппла
    procedure PressEventProc(Sender: TObject; GroupID, ButtonID: integer);
    procedure UnPressEventProc(Sender: TObject; GroupID, ButtonID: integer);
    procedure SetAddressPoint(Sender : TObject);
  end;

  TRouteType = (rtBus, rtTrolley, rtTram);
  TRouteNodeType = (rntRoute, rntBranch, rntBranchStreet, rntStreet);

  TRouteNGPTNodeData = record
    okey: string;
    routeCode: string;
    lineName: string;
    routeNumber: string;
    routeType: TRouteType;
  end;

  TResultNodeData = record
    nodeType: TRouteNodeType;
    okey: integer;
    MUID: int64;
    Name: string;
    Distance: double;
  end;

  PRouteNGPTNodeData = ^TRouteNGPTNodeData;
  PResultNodeData = ^TResultNodeData;

  TTestGraphNode = class( TBasicGraphNode )
  private
    FOKEY: Integer;
  public

    property OKEY: Integer read FOKEY;
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

function GetGraph(conn: TDBAConnection): TBasicGraph;
var
  queryString: string;
  dbResult_Nodes, dbResult_Edges: TDBResult;
begin
  Result := TbasicGraph.Create();

  queryString := 'SELECT ' + conn.quoteName( 'MUID' ) + ' FROM ' + conn.quoteName( 'uds' )
                  + '.' + conn.quoteName( 'graph_nodes' ) + ' gn WHERE gn.sign_deleted = 0';

  conn.QueryOpen( queryString, dbResult_Nodes, true );

  queryString := 'SELECT ' + conn.quoteName( 'MUID' ) + ', ' + conn.quoteName( 'startNodeMUID' )
                  + ', ' + conn.quoteName( 'endNodeMUID' ) + ', ' + conn.quoteName( 'ObjectLength' ) + ' FROM '
                  + conn.quoteName( 'uds' ) + '.' + conn.quoteName( 'graph_sections' ) + 'gs WHERE gs.sign_deleted = 0';

  conn.QueryOpen( queryString, dbResult_Edges, true );

  while dbResult_Nodes.Fetch() do
    Result.AddNode( dbResult_Nodes.asInt64( 0 ) );

  while dbResult_Edges.Fetch() do
  begin
    if ( Result.GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ) <> nil )
      AND ( Result.GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ) <> nil ) then
      Result.AddEdge( Result.GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ),
                    Result.GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ),
                    dbResult_Edges.asInt64( 0 ),
                    dbResult_Edges.asFloat( 3 ) );
  end;

  FreeAndNil( dbResult_Nodes );
  FreeAndNil( dbResult_Edges );
end;

{**********************************************************************************************
* FormCreate
***********************************************************************************************}
procedure TFMain.FormCreate(Sender: TObject);
var
  aliasIniFilename, ConnString, sql : string;
  dbResult : TDBResult;
begin
  FMap := TFMap.Create(nil);

  //Алиасы для соединения с БД
  aliasIniFilename := ChangeFileExt(Application.ExeName, '.ini');
  IniAliasOptions := TIniAliasOptions.Create();
  IniAliasOptions.LoadFromFile(aliasIniFilename);

  //Подключение к БД Uran 3307
  ConnString:=IniAliasOptions.Aliases.itemsByKey['main'];
  ConnUran3312:=TDbaConnection.CreateByParams(ConnString);
  ConnUran3312.connect();
  mapStreets := TMapStrings.Create();
  mapStartAddresses := TMapStrings.Create();
  mapEndAddresses := TMapStrings.Create();

  GroupId:=FMap.Mappl.RegisterMapModeButtonGroup(PressEventProc, UnPressEventProc); //регистрируем группу в МАППЛЕ
  StartNodeButtonId:=FMap.Mappl.RegisterMapModeButton(GroupId);                     //регистрируем режим кнопки в группе
  EndNodeButtonId:=FMap.Mappl.RegisterMapModeButton(GroupId);                       //регистрируем режим кнопки в группе

  //регистрируем режимы карты в МАППЛЕ
  StartNodeMapMode:=FMap.Mappl.RegisterMapMode;
  EndNodeMapMode:=FMap.Mappl.RegisterMapMode;

  //Присваиваем значения глобальным переменным в FMap
  FMap.StartNodeMode := StartNodeMapMode;
  FMap.EndNodeMode := EndNodeMapMode;

  //Режимы являются поисковыми
  FMap.Mappl.UserMapModeTool[StartNodeMapMode]:=umkSearch;
  FMap.Mappl.UserMapModeTool[EndNodeMapMode]:=umkSearch;

  //Подключение к БД
  ConnString:=IniAliasOptions.Aliases.itemsByKey['main'];
  Conn:=TDbaConnection.CreateByParams(ConnString);
  Conn.connect();

  fillStreets();

  //Инициализация кэшей
  FGraph := GetGraph( Conn );

  //Cache := TCache.Create();
  //Cache.MapObjects := TMapObjects.Create();
  //Cache.init(Conn);

  //VertexCache := TVertexCache.Create();
  //VertexCache.init(Conn);

  //Выделение памяти для VertexValues
  //sql := 'SELECT MAX(okey) FROM uds.graph_nodes';
  //Conn.QueryOpen(sql, dbResult, true);
  //dbResult.Fetch();
  //SetLength(VertexValues, dbResult.asInteger(0) + 1);

  //FreeAndNil(dbResult);                            

  ResultMosList := TMapObjectStructureList.Create(true);
  VTResult.NodeDataSize := sizeOf(TResultNodeData);

  //Создание класса временных объектов
  TempObjects:=FMap.Mappl.CreateTempObjects;
  TempObjects.StylePenColor[0]:=clRED;
  TempObjects.StylePenWidth[0]:=3;

  //Создание класса временных объектов для FMap
  FMap.TempObjects:=FMap.Mappl.CreateTempObjects;
  FMap.TempObjects.StylePenColor[0]:=clRed;
  FMap.TempObjects.StylePenWidth[0]:=10;
  FMap.TempObjects.StylePenColor[1]:=clBlue;
  FMap.TempObjects.StylePenWidth[1]:=10;

  FMap.TempObjects.LoadStyleLibrary('MAPPROJECT\MSS\JUNCS_EDIT.MSS');



  FMap.FromIndex := -1;
  FMap.ToIndex := -1;
  flDelStartPoint := true;
  flDelEndPoint := true;

  //Цепляем событие
  FMap.OnSetPoint := SetAddressPoint;

  //Подключаем алиасы, загружаем проект и показываем карту
  FMap.Mappl.Map.MapplConnections.Resolver.Aliases := IniAliasOptions.Aliases;
  FMap.Mappl.MapProject := 'MapProject\TransportPosterPrint.mpj';
  FMap.Mappl.ShowMap();

  BSetStartAddress.Enabled := false;
  BSetEndAddress.Enabled := false;
  BCalculate.Enabled := false;

  FMap.Show();

end;

{**********************************************************************************************
* fillStreets - заполнение значений комбобоксов "Улица"
***********************************************************************************************}
procedure TFMain.fillStreets();
var
  sql : string;
  dbResult : TDBResult;
begin
  mapStreets.Clear();
  CBStartStreet.Items.Clear();
  CBEndStreet.Items.Clear();

  sql := 'SELECT muid, name FROM asot.oas_streets'
   + ' WHERE NAME <> "" and NOT ISNULL( NAME ) '
   + ' GROUP BY NAME'
   + ' ORDER BY IF(name = "", "яяя", name) ';

  if (Conn.QueryOpen(sql, dbResult, true) <> 0) then
    showDialog(dtError,dbsOK,'Ошибка при выполнении sql-запроса: ' + sql);

  mapStreets.addItem('0', '0');
  CBStartStreet.Items.Add('');
  CBEndStreet.Items.Add('');

  while dbResult.Fetch() do
  begin
    mapStreets.addItem(dbResult.asString(0), dbResult.asString(1));
    CBStartStreet.Items.Add(dbResult.asString(1));
    CBEndStreet.Items.Add(dbResult.asString(1));
  end;

  dbResult.Free();

  CBStartStreet.ItemIndex := 0;
  CBEndStreet.ItemIndex := 0;
end;

{**********************************************************************************************
* CBStartStreetChange
***********************************************************************************************}
procedure TFMain.CBStartStreetChange(Sender: TObject);
var
  value : string;
begin
  if CBStartStreet.itemIndex = -1 then
    exit;

  value := mapStreets.keys[CBStartStreet.itemIndex];

  if value = '0' then
  begin
    mapStartAddresses.Clear();
    CBStartHouse.Items.Clear();
    CBStartHouse.ItemIndex := -1;
    CBStartHouseChange(nil);
    exit;
  end;

  fillStartAddresses(value);
  CBStartHouseChange(nil);
end;

{**********************************************************************************************
* fillStartAddresses - заполнение значений комбобокса "Дом" (начало пути)
***********************************************************************************************}
procedure TFMain.fillStartAddresses(aStreetMUID: string);
var
  sql : string;
  dbResult : TDBResult;
begin
  mapStartAddresses.Clear();
  CBStartHouse.Items.Clear();

{  sql := 'SELECT a.muid, a.full_number'
    + ' FROM egko.building a'
    + ' WHERE a.street_name = ' + ConnUran3312.quoteValue(aStreetName)
    + ' ORDER BY Length(cast(a.full_number as unsigned)), a.full_number' ;}

  sql := 'SELECT b.muid, a.full_number ' +
         ' FROM asot.oas_addresses a INNER JOIN asot.kas_buildings b ON b.main_address_muid = a.muid' +
         ' WHERE a.street_muid = ' + aStreetMUID +
         ' GROUP BY a.muid' +
         ' ORDER BY Length(cast(a.full_number as unsigned)), a.full_number';

  if (Conn.QueryOpen(sql, dbResult, true) <> 0) then
    showDialog(dtError,dbsOK,'Ошибка при выполнении sql-запроса: ' + sql);

  mapStartAddresses.addItem('0', '0');
  CBStartHouse.Items.Add('');

  while dbResult.Fetch() do
  begin
    mapStartAddresses.addItem(dbResult.asString(0), dbResult.asString(1));
    CBStartHouse.items.Add(dbResult.asString(1));
  end;

  dbResult.Free();
  CBStartHouse.ItemIndex := 0;
end;

{**********************************************************************************************
* CBEndStreetChange
***********************************************************************************************}
procedure TFMain.CBEndStreetChange(Sender: TObject);
var
  value : string;
begin
  if CBEndStreet.itemIndex = -1 then
    exit;

  value := mapStreets.keys[CBEndStreet.itemIndex];
  if value = '0' then
  begin
    mapEndAddresses.Clear();
    CBEndHouse.Items.Clear();
    CBEndHouse.ItemIndex := -1;
    CBEndHouseChange(nil);
    exit;
  end;

  fillEndAddresses(value);
  CBEndHouseChange(nil);
end;

{**********************************************************************************************
* fillEndAddresses - заполнение значений комбобокса "Дом" (конец пути)
***********************************************************************************************}
procedure TFMain.fillEndAddresses(aStreetMUID: string);
var
  sql : string;
  dbResult : TDBResult;
begin
  mapEndAddresses.Clear();
  CBEndHouse.Items.Clear();
{
  sql := 'SELECT a.muid, full_number'
    + ' FROM egko.building a'
    + ' WHERE a.street_muid = ' + ConnUran3312.quoteValue(aStreetName)
    + ' ORDER BY Length(cast(a.full_number as unsigned)), a.full_number' ;  }

  sql := 'SELECT b.muid, a.full_number ' +
         ' FROM asot.oas_addresses a INNER JOIN asot.kas_buildings b ON b.main_address_muid = a.muid' +
         ' WHERE a.street_muid = ' + aStreetMUID +
         ' GROUP BY a.muid' +
         ' ORDER BY Length(cast(a.full_number as unsigned)), a.full_number';

  if (Conn.QueryOpen(sql, dbResult, true) <> 0) then
    showDialog(dtError,dbsOK,'Ошибка при выполнении sql-запроса: ' + sql);

  mapEndAddresses.addItem('0', '0');
  CBEndHouse.Items.Add('');

  while dbResult.Fetch() do
  begin
    mapEndAddresses.addItem(dbResult.asString(0), dbResult.asString(1));
    CBEndHouse.items.Add(dbResult.asString(1));
  end;

  dbResult.Free();
  CBEndHouse.ItemIndex := 0;
end;

{**********************************************************************************************
* CBStartHouseChange
***********************************************************************************************}
procedure TFMain.CBStartHouseChange(Sender: TObject);
begin
  FStartBuildingMuid := '';

  if flDelStartPoint Then
    FMap.DeleteStartPoint();

  if CBStartHouse.itemindex > 0 then
    FStartBuildingMuid := mapStartAddresses.keys[CBStartHouse.itemindex];

  BSetStartAddress.Enabled := (FStartBuildingMuid <> '');
  BCalculate.Enabled := (FMap.FromIndex <> -1) and (FMap.ToIndex <> -1);
end;

{**********************************************************************************************
* CBEndHouseChange
***********************************************************************************************}
procedure TFMain.CBEndHouseChange(Sender: TObject);
begin
  FEndBuildingMuid := '';

  if flDelEndPoint Then
    FMap.DeleteEndPoint();

  if CBEndHouse.itemindex > 0 then
    FEndBuildingMuid := mapEndAddresses.keys[CBEndHouse.itemindex];

  BSetEndAddress.Enabled := (FEndBuildingMuid <> '');
  BCalculate.Enabled := (FMap.FromIndex <> -1) and (FMap.ToIndex <> -1);
end;

{**********************************************************************************************
* PressEventProc
***********************************************************************************************}
procedure TFMain.PressEventProc(Sender: TObject; GroupID,ButtonID: integer);
begin
  If ButtonId = StartNodeButtonId Then
  begin
    BGetStartAddressFromMap.Down:=true;
    FMap.Mappl.MapMode:=StartNodeMapMode;
  end
  else
  If ButtonId = EndNodeButtonId Then
  begin
    BGetEndAddressFromMap.Down:=true;
    FMap.Mappl.MapMode:=EndNodeMapMode;
  end;
end;

{**********************************************************************************************
* UnPressEventProc
***********************************************************************************************}
procedure TFMain.UnPressEventProc(Sender: TObject; GroupID,ButtonID: integer);
begin
  BGetStartAddressFromMap.Down := false;
  BGetEndAddressFromMap.Down := false;
end;

{**********************************************************************************************
* BGetStartAddressFromMapClick
***********************************************************************************************}
procedure TFMain.BGetStartAddressFromMapClick(Sender: TObject);
begin
  // показываем окно карты
  if BGetStartAddressFromMap.Down then
  begin
    if not (fsVisible in FMap.FormState) then
      FMap.Show();
  BGetEndAddressFromMap.Down := False;
  FMap.Mappl.MapModeButtonDown(StartNodeButtonId,StartNodeMapMode);
  end;

end;

{**********************************************************************************************
* BGetEndAddressFromMapClick
***********************************************************************************************}
procedure TFMain.BGetEndAddressFromMapClick(Sender: TObject);
begin
  // показываем окно карты
  if BGetEndAddressFromMap.Down then
  begin
    if not (fsVisible in FMap.FormState) then
      FMap.Show();
  BGetStartAddressFromMap.Down := False;
  FMap.Mappl.MapModeButtonDown(EndNodeButtonId,EndNodeMapMode);
  end;
end;

{**********************************************************************************************
* BCalculateClick - Расчёт кратчайшего пути
***********************************************************************************************}
procedure TFMain.BCalculateClick(Sender: TObject);
var
    way : Array of Integer;
    wayAmount, currentLength, i, currMinWay: integer;
    startNodeMUID, endNodeMUID : Int64;
    MOS : TMapObjectStructure;
    Time : TDateTime;
    Milliseconds : Double;
    TimeSpent : Double;
    path: TList;
    len: double;
    index: integer;
    query: String;
    dbRes: TDBResult;

//Нажатие кнопки--------------
begin
  If not GetStartOkey(startNodeMUID, conn) Then
    begin
      showDialog(dtError,dbsOK,'Не удалось построить начальную вершину!');
      exit;
    end;

  If not GetEndOkey(endNodeMUID, conn) Then
    begin
      showDialog(dtError,dbsOK,'Не удалось построить конечную вершину!');
      exit;
    end;

  ResultMosList.Clear();
  VTResult.Clear();

  If HighlightTheSegmentOnly() Then Exit;

  BCalculate.Enabled := false;

  //Инициализация начальных данных

  MOS := TMapObjectStructure.Create();
  TempObjects.Clear();

  currentLength := 0;
  wayAmount := 0;

  TimeSpent := 0;

  Time := Now();

  //Поиск пути
  path := TList.Create();

  len := 0;
  len := FGraph.FindPath( FGraph.GetNode_ByUID( startNodeMUID ), FGraph.GetNode_ByUID( endNodeMUID ), path);

  Milliseconds := DiffDateTimeMsec(Now(),Time) / 1000;

  if ( path <> nil ) and ( path.count > 0 ) then
  begin

    wayAmount := path.Count + 1;
    SetLength( way, wayAmount );
    for index := 0 to path.count - 1 do
    begin
      query := 'SELECT OKEY FROM uds.graph_nodes WHERE MUID = ' + conn.quoteValue( IntToStr( TBasicGraphEdge( path[index] ).NodeFrom.UID ) );
      conn.QueryOpen( query, dbRes, true );
      dbRes.Fetch();
      way[ wayAmount - 1 - index] := dbRes.asInteger( 0 );
      FreeAndNil(dbRes);
    end;
    query := 'SELECT OKEY FROM uds.graph_nodes WHERE MUID = ' + conn.quoteValue( IntToStr( TBasicGraphEdge( path[path.Count - 1] ).NodeTo.UID ) );
    conn.QueryOpen( query, dbRes, true );
    dbRes.Fetch();
    way[ 0 ] := dbRes.asInteger( 0 );
    FreeAndNil(dbRes);

    //Посветка дуг на карте
    highlightTheWay(Conn, way, wayAmount);

  end;

  status.Panels[0].Text := 'Время поиска: ' + FloatToStr(Milliseconds) + 'c';
  status.Panels[1].Text := 'Длина пути: ' + FloatToStr(len) + ' метров';

  FillVTResult();
  FMap.Show();
  FMap.Mappl.FitToScreenByMOSList(ResultMosList);

  BCalculate.Enabled := true;

  //Освобождение памяти
  FreeAndNil(MOS);
  FreeAndNil(path);

end;

{**********************************************************************************************
* GetStartOkey
***********************************************************************************************}
function TFMain.GetStartOkey(var nodeMUID: Int64; conn: TDBAConnection) : boolean;
var
  oAmount, oIndex, nSubObject, LastPointNumber : Integer;
  resultX, resultY, distance : Double;
  queryString: string;
  dbRes: TDBResult;
begin
  Result := False;
  oAmount := FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XFrom, FMap.YFrom,
  oIndex, nSubObject, LastPointNumber, resultX, resultY, distance);

  If oAmount < 0 Then
    Exit;

  queryString := 'SELECT MUID FROM uds.graph_sections WHERE OKEY = ' + conn.quoteValue( IntToStr( oIndex ) );
  conn.QueryOpen( queryString, dbRes, true );
  if (dbRes.Fetch()) then
  begin
    nodeMUID := FGraph.GetEdge_ByUID( dbRes.asInt64( 0 ) ).NodeTo.UID;
    Result := true;
  end
  else
    Result := false;
end;

{**********************************************************************************************
* GetEndOkey
***********************************************************************************************}
function TFMain.GetEndOkey(var nodeMUID : Int64; conn: TDBAConnection) : boolean;
var
  oAmount, oIndex, nSubObject, LastPointNumber : Integer;
  resultX, resultY, distance : Double;
  queryString: string;
  dbRes: TDBResult;
begin
  Result := False;
  oAmount := FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XTo, FMap.YTo,
  oIndex, nSubObject, LastPointNumber, resultX, resultY, distance);

  If oAmount < 0 Then
    Exit;

  queryString := 'SELECT MUID FROM uds.graph_sections WHERE OKEY = ' + conn.quoteValue( IntToStr( oIndex ) );
  conn.QueryOpen( queryString, dbRes, true );;
  if (dbRes.Fetch()) then
  begin
    nodeMUID := FGraph.GetEdge_ByUID( dbRes.asInt64( 0 ) ).NodeFrom.UID;
    Result := true;
  end
  else
    Result := false;
end;

{**********************************************************************************************
* HighlightTheSegmentOnly - подсвечивать только отрезок дуги
***********************************************************************************************}
function TFMain.HighlightTheSegmentOnly() : boolean;
var
  oIndexFrom, oIndexTo, nSubObject, LastPointNumberFrom, LastPointNumberTo, Count, i0, i1 : Integer;
  resultXFrom, resultYFrom, resultXTo, resultYTo, distance : Double;
  MOS, TempMos : TMapObjectStructure;
  newVertex : TDoublePoint;
begin
  Result := False;

  FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XFrom, FMap.YFrom,
  oIndexFrom, nSubObject, LastPointNumberFrom, resultXFrom, resultYFrom, distance);

  FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XTo, FMap.YTo,
  oIndexTo, nSubObject, LastPointNumberTo, resultXTo, resultYTo, distance);

  If (oIndexTo <> oIndexFrom) or (LastPointNumberFrom > LastPointNumberTo) Then
    Exit;

  TempObjects.Clear;

  MOS := TMapObjectStructure.Create;
  TempMOS := TMapObjectStructure.Create;
  MOS.oType := MOS_POLYLINE;

  MOS.AddSubObject();

  newVertex.x := FMap.XFrom;
  newVertex.y := FMap.YFrom;
  MOS.AddVertex(0,newVertex);

  newVertex.x := resultXFrom;
  newVertex.y := resultYFrom;
  MOS.AddVertex(0,newVertex);

  Count:=FMap.Mappl.ReadMapObject(TempMOS,LAYER_CODE_EDGES,oIndexFrom);
  if Count > 0 then
  begin
    for i0:=0 to TempMOS.SubObjectsCount-1 do
    begin
      for i1:=LastPointNumberFrom + 1 to LastPointNumberTo do
      begin
        newVertex.x := TempMOS.X[i0,i1];
        newVertex.y := TempMOS.Y[i0,i1];
        MOS.AddVertex(0, newVertex);
      end;
    end;
  end;

  newVertex.x := resultXTo;
  newVertex.y := resultYTo;
  MOS.AddVertex(0,newVertex);

  newVertex.x := FMap.XTo;
  newVertex.y := FMap.YTo;
  MOS.AddVertex(0,newVertex);

  TempObjects.AddPolyLineObjectByMOS(MOS, dwoLineSimple, 0);
  ResultMosList.Add(MOS);

  status.Panels[1].Text := 'Длина: ' + FloatToStr(Round(MOS.GetLength)/1000) + ' метров';

  //FreeAndNil(MOS);
  FreeAndNil(TempMOS);

  FMap.Mappl.FitToScreenByMOSList(ResultMosList);
  FMap.Mappl.RefreshMapOverlay();

  Result := True;
  FillVTResult();
end;

{**********************************************************************************************
* highlightTheWay - Функция поиска и подсветки дуг пути
***********************************************************************************************}
procedure TFMain.HighlightTheWay(Conn: TDbaConnection; way : Array of Integer; wayAmount : Integer);
var dbResult : TDBResult;
    i, Count, i0, i1: Integer;
    sql : string;
    line : PChar;
    MOS, TempMOS : TMapObjectStructure;
    oIndex, nSubObject, LastPointNumber : Integer;
    resultX, resultY, distance : Double;
    newVertex : TDoublePoint;
begin
  //If wayAmount < 2 Then Exit;

  MOS := TMapObjectStructure.Create;
  TempMOS := TMapObjectStructure.Create;

  FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XFrom, FMap.YFrom,
  oIndex, nSubObject, LastPointNumber, resultX, resultY, distance);

  MOS.oType := MOS_POLYLINE;
  MOS.AddSubObject();

  newVertex.x := FMap.XFrom;
  newVertex.y := FMap.YFrom;
  MOS.AddVertex(0,newVertex);

  newVertex.x := resultX;
  newVertex.y := resultY;
  MOS.AddVertex(0,newVertex);

  Count:=FMap.Mappl.ReadMapObject(TempMOS,LAYER_CODE_EDGES,oIndex);
  if Count > 0 then
  begin
    for i0:=0 to TempMOS.SubObjectsCount-1 do
    begin
      for i1:=LastPointNumber + 1 to TempMOS.VertexesCount[i0]-1 do
      begin
        newVertex.x := TempMOS.X[i0,i1];
        newVertex.y := TempMOS.Y[i0,i1];
        MOS.AddVertex(0, newVertex);
      end;
    end;
  end;

  TempObjects.AddPolyLineObjectByMOS(MOS, dwoLineSimple, 0);
  totalWayLength := totalWayLength + Round(MOS.GetLength);

  ResultMosList.Add(MOS);
  //FreeAndNil(MOS);
  FreeAndNil(TempMOS);

  For i := wayAmount - 1 downto 1 do
  begin
    sql := 'SELECT LINE FROM uds.graph_sections WHERE NodeIn = ' + IntToStr(way[i]) + ' AND NodeOut = ' + IntToStr(way[i-1]);
    Conn.QueryOpen(sql, dbResult, true);
    If dbResult.Fetch() Then
    begin
      dbResult.asBlob(0,line);
      MOS := TMapObjectStructure.Create;
      MOS.oType := MOS_POLYLINE;
      MOS.GetFromBuf( PByte(line), -1, 0, 0, 0, 1 );
      TempObjects.AddPolyLineObjectByMOS(MOS, dwoLineSimple, 0);
      ResultMosList.Add(MOS);
      //FreeAndNil(MOS);
      FreeAndNil(dbResult);
    end;
  end;

  MOS := TMapObjectStructure.Create;
  TempMOS := TMapObjectStructure.Create;

  FMap.FMappl.GetNearestPointOfLayer(LAYER_CODE_EDGES, 1000000, FMap.XTo, FMap.YTo,
  oIndex, nSubObject, LastPointNumber, resultX, resultY, distance);

  MOS.oType := MOS_POLYLINE;
  MOS.AddSubObject();

  Count:=FMap.Mappl.ReadMapObject(TempMOS,LAYER_CODE_EDGES,oIndex);
  if Count > 0 then
  begin
    for i0:=0 to TempMOS.SubObjectsCount-1 do
    begin
      for i1:= 0 to LastPointNumber do
      begin
        newVertex.x := TempMOS.X[i0,i1];
        newVertex.y := TempMOS.Y[i0,i1];
        MOS.AddVertex(0, newVertex);
      end;
    end;
  end;

  newVertex.x := resultX;
  newVertex.y := resultY;
  MOS.AddVertex(0,newVertex);

  newVertex.x := FMap.XTo;
  newVertex.y := FMap.YTo;
  MOS.AddVertex(0,newVertex);

  TempObjects.AddPolyLineObjectByMOS(MOS, dwoLineSimple, 0);
  totalWayLength := totalWayLength + Round(MOS.GetLength);

  ResultMosList.Add(MOS);
  //FreeAndNil(MOS);
  FreeAndNil(TempMOS);

  FMap.Mappl.RefreshMapOverlay();

end;


{**********************************************************************************************
* SetAddressPoint - обработчик события
***********************************************************************************************}
procedure TFMain.SetAddressPoint(Sender: TObject);
var
  resObjects : TLayerObjectIdentificationList;
  delta : double;
  sql, addressMuid, streetMuid, fullNumber : string;
  dbResult: TDBResult;
begin
  if not (Sender is TFMap) then Exit;

  If FMap.Mappl.MapMode = StartNodeMapMode Then
  begin
    flDelStartPoint := false;
    CBStartHouse.ItemIndex := 0;

    // поиск объектов
    resObjects := TLayerObjectIdentificationList.Create();
    delta := 2 * FMap.mappl.mapZoom;
    FMap.Mappl.GetLayerObjectsInMapPoint(LAYER_CODE_BUILDING, FMap.XFrom, FMap.YFrom, resObjects, delta, ofmpDefault, false, true, ctReal, '', nil, true);

    if (resObjects.Count > 0) then
    begin
      sql := 'SELECT b.muid, a.full_number, a.street_muid'
        + ' FROM asot.kas_buildings b'
        + ' LEFT JOIN asot.oas_addresses a ON b.main_address_muid = a.muid '
        + ' WHERE b.muid = ' + IntToStr(resObjects[0].oMUID);

      if (Conn.QueryOpen(sql, dbResult, true) <> 0) then
        showDialog(dtError,dbsOK,'Ошибка при выполнении sql-запроса: ' + sql);

      if dbResult.Fetch() then
      begin
        addressMuid := dbResult.asString(0);
        fullNumber :=  dbResult.asString(1);
        streetMuid := dbResult.asString(2);

        if (addressMuid <> '') and (streetMuid <> '') then
        begin
          CBStartStreet.ItemIndex := mapStreets.IndexOf(streetMuid);
          CBStartStreetChange(nil);
          CBStartHouse.ItemIndex := mapStartAddresses.IndexOf(addressMuid);
          CBStartHouseChange(nil);
        end
        else
        begin          // нет адреса для данного строения - сбрасываем значения.
          CBStartStreet.ItemIndex := 0;
          CBStartStreet.Text := 'Точка на карте';
          CBStartStreetChange(nil);
          CBStartHouse.ItemIndex := 0;
          CBStartHouse.Text := 'Точка на карте';
          CBStartHouseChange(nil);
        end;
      end;
      dbResult.Free();
    end
    else
      begin
          CBStartStreet.ItemIndex := 0;
          CBStartStreet.Text := 'Точка на карте';
          CBStartHouse.ItemIndex := 0;
          CBStartHouse.Text := 'Точка на карте';
          CBStartHouseChange(nil);
        end;

    flDelStartPoint := true;
    resObjects.Free();
  end;

  If FMap.Mappl.MapMode = EndNodeMapMode Then
  begin
    flDelEndPoint := false;
    CBEndHouse.ItemIndex := 0;

    // поиск объектов
    resObjects := TLayerObjectIdentificationList.Create();
    delta := 2 * FMap.mappl.mapZoom;
    FMap.Mappl.GetLayerObjectsInMapPoint(LAYER_CODE_BUILDING, FMap.XTo, FMap.YTo, resObjects, delta, ofmpDefault, false, true, ctReal, '', nil, true);

    if (resObjects.Count > 0) then
    begin
      sql := 'SELECT b.muid, a.full_number, a.street_muid'
        + ' FROM asot.kas_buildings b'
        + ' LEFT JOIN asot.oas_addresses a ON b.main_address_muid = a.muid '
        + ' WHERE b.muid = ' + IntToStr(resObjects[0].oMUID);

      if (Conn.QueryOpen(sql, dbResult, true) <> 0) then
        showDialog(dtError,dbsOK,'Ошибка при выполнении sql-запроса: ' + sql);

      if dbResult.Fetch() then
      begin
        addressMuid := dbResult.asString(0);
        fullNumber :=  dbResult.asString(1);
        streetMuid := dbResult.asString(2);

        if (addressMuid <> '') and (streetMuid <> '') then
        begin
          CBEndStreet.ItemIndex := mapStreets.IndexOf(streetMuid);
          CBEndStreetChange(nil);
          CBEndHouse.ItemIndex := mapEndAddresses.IndexOf(addressMuid);
          CBEndHouseChange(nil);
        end
        else
        begin          // нет адреса для данного строения - сбрасываем значения.
          CBEndStreet.ItemIndex := 0;
          CBEndStreet.Text := 'Точка на карте';
          CBEndStreetChange(nil);
          CBEndHouse.ItemIndex := 0;
          CBEndHouse.Text := 'Точка на карте';
          CBEndHouseChange(nil);
        end;
      end;
      dbResult.Free();
    end
    else
      begin
          CBEndStreet.ItemIndex := 0;
          CBEndStreet.Text := 'Точка на карте';
          CBEndHouse.ItemIndex := 0;
          CBEndHouse.Text := 'Точка на карте';
          CBEndHouseChange(nil);
        end;

    flDelEndPoint := true;
    resObjects.Free();
  end;

end;

{**********************************************************************************************
* BSetStartAddressClick
***********************************************************************************************}
procedure TFMain.BSetStartAddressClick(Sender: TObject);
var
  okey : Integer;
  xCenter, yCenter : Double;
begin
  If FStartBuildingMuid = '' Then
    begin
      showDialog(dtError,dbsOK,'Не удалось выбрать адрес!');
      exit;
    end;

  okey := FMap.Mappl.GetObjectIndexByMUID(LAYER_CODE_BUILDING, StrToInt64(FStartBuildingMuid));
  if FMap.Mappl.GetMapObjectCenter(LAYER_CODE_BUILDING, okey, xCenter, yCenter) Then
  begin
    FMap.SetStartPoint(xCenter, yCenter);
    FMap.Mappl.MapCenterX := xCenter;
    FMap.Mappl.MapCenterY := YCenter;
    FMap.Mappl.MapScale := 1000;
    FMap.Mappl.RefreshMap;
    FMap.Show();
  end;

  BCalculate.Enabled := (FMap.FromIndex <> -1) and (FMap.ToIndex <> -1);
end;

{**********************************************************************************************
* BSetEndAddressClick
***********************************************************************************************}
procedure TFMain.BSetEndAddressClick(Sender: TObject);
var
  okey : Integer;
  xCenter, yCenter : Double;
begin
  If FEndBuildingMuid = '' Then
    begin
      showDialog(dtError,dbsOK,'Не удалось выбрать адрес!');
      exit;
    end;

  okey := FMap.Mappl.GetObjectIndexByMUID(LAYER_CODE_BUILDING, StrToInt64(FEndBuildingMuid));
  if FMap.Mappl.GetMapObjectCenter(LAYER_CODE_BUILDING, okey, xCenter, yCenter) Then
  begin
    FMap.SetEndPoint(xCenter, yCenter);
    FMap.Mappl.MapCenterX := xCenter;
    FMap.Mappl.MapCenterY := YCenter;
    FMap.Mappl.MapScale := 1000;
    FMap.Mappl.RefreshMap;
    FMap.Show();
  end;

  BCalculate.Enabled := (FMap.FromIndex <> -1) and (FMap.ToIndex <> -1);
end;

{**********************************************************************************************
* FormDestroy
***********************************************************************************************}
procedure TFMain.FormDestroy(Sender: TObject);
begin
  FMap.Mappl.CloseMap();
  //FreeAndNil(FMap);
  ResultMosList.Clear();
  FreeAndNil(ConnUran3312);
  FreeAndNil(Conn);
end;


{--------------------------------------Функции работы с TreeView----------------------------------}
{*******************************************************************************
* FillVTResult
*******************************************************************************}
procedure TFMain.FillVTResult();
var
  ParentNode: PVirtualNode;
  Data: PResultNodeData;
// wayData : TMapObjectStructureList;
//  StreetData : PResultNodeData;

begin
     // заполняем VTResult
  ParentNode := VTResult.AddChild(nil);
  Data :=  VTResult.GetNodeData(ParentNode);

  Data.Name := 'Кратчайший путь';
  Data.okey       := 0;
  Data.MUID       := -1;
  Data.nodeType   := rntRoute;

  PrepareResultData (ParentNode);


  filterResultTree(VTResult.RootNode);
end;

{*******************************************************************************
* PrepareResultData
*******************************************************************************}
procedure TFMain.PrepareResultData (ParentNode: PVirtualNode);
var
  i, j, subObjectCount : integer;
  MUIDRoad :int64;
  tempMOS:  TMapObjectStructure;
  curDistance, totalDistance: double;
  streetName: string;

  //Conn: TDBAConnection;
  sql: string;
  dbResult: TDBResult;

  BranchNode, StreetNode, StreetBranchNode: PVirtualNode;
  Data, BranchData, StreetData, LastStreetBranchData, LastStreetData: PResultNodeData;
  
begin
  tempMOS := TMapObjectStructure.Create();
  //Conn := TDbaConnection.CreateByParams ('driver=MySql;hostname=192.168.200.5;port=3307;user=asot;password=asot;collation=cp1251_general_ci;charset=cp1251');

  totalDistance := 0;

  LastStreetData := nil;

  for i := 0 to ResultMosList.Count -1 do
  begin
    LastStreetBranchData := nil;

    BranchNode := VTResult.AddChild(ParentNode);
    BranchData :=  VTResult.GetNodeData(BranchNode);

    BranchData.Name       := 'Дуга №' + IntToStr(i);
    BranchData.okey       := i;
    BranchData.MUID       := ResultMosList[i].oMUID;
    BranchData.Distance   := ResultMosList[i].GetLength() * FMap.Mappl.MapUnitsCoef / 1000;
    BranchData.nodeType   := rntBranch;

    FMap.Mappl.IntersectMOSContourByLayer (ResultMosList[i], LAYER_CODE_UDS_ROAD, tempMOS); // должна возвращать !

    if (tempMOS = nil) then
      continue;

    subObjectCount := tempMOS.SubObjectsCount;

    for j := 0 to subObjectCount -1 do
    begin
      if tempMOS.ID[j] <= 1 then
        continue;

      MUIDRoad := FMap.Mappl.GetMUIDByObjectIndex(LAYER_CODE_UDS_ROAD, tempMOS.ID[j]);

      // Считаем длину улицы на дуге
      tempMOS.oType := 1;
      curDistance := tempMOS.GetSubObjectLength(j) * FMap.Mappl.MapUnitsCoef / 1000;

      // Получаем наименование улицы, соответствующее данному участку дороги
      sql := 'SELECT b.name FROM ' + ROADS + ' a JOIN ' +  STREETS + ' b ON a.street_muid = b.muid WHERE a.MUID = ' + IntToStr (MUIDRoad);

      if (Conn.QueryOpen(sql, dbResult, false) <> 0) then
        raise Exception.Create('Не удалось считать названия улиц.');

      streetName := '';

      if dbResult.Fetch() then
        streetName := dbResult.asString(0);

      FreeAndNil(dbResult);

      // Смотрим, требуется ли добавлять улицу в дерево
      {if curDistance <= MAX_ROAD_WIDTH then
        continue;}

      // Если последняя улица соответствует данной, то в список ее не добавляем
      if (LastStreetBranchData <> nil) and (LastStreetBranchData.Name = streetName) then
        LastStreetBranchData.Distance := LastStreetBranchData.Distance + curDistance
      else
      begin
        StreetBranchNode := VTResult.AddChild(BranchNode);
        LastStreetBranchData :=  VTResult.GetNodeData(StreetBranchNode);

        if (streetName <> '') then
          LastStreetBranchData.Name := streetName
        else
          LastStreetBranchData.Name := 'Без названия';

        LastStreetBranchData.okey       := tempMOS.ID[j];
        LastStreetBranchData.MUID       := MUIDRoad;
        LastStreetBranchData.Distance   := curDistance;
        LastStreetBranchData.nodeType   := rntBranchStreet;
      end;

      // Считаем общую длину
      totalDistance := totalDistance + curDistance;

      // Если последний узел улиц соответствует текущей улице, то увеличиваем ее длину
      if (LastStreetData <> nil) and
         ( (LastStreetData.Name = streetName) or
           ( (streetName = '') and (LastStreetData.Name = 'Без названия') ) ) then
        LastStreetData.Distance := LastStreetData.Distance + curDistance
      else
      begin
        StreetNode := VTResult.AddChild(ParentNode);
        StreetData :=  VTResult.GetNodeData(StreetNode);

        if (streetName <> '') then
          StreetData.Name := streetName
        else
          StreetData.Name := 'Без названия';

        StreetData.okey       := tempMOS.ID[j];
        StreetData.MUID       := MUIDRoad;
        StreetData.Distance   := curDistance;
        StreetData.nodeType   := rntStreet;

        LastStreetData := StreetData;
      end;
    end;
  end;

  FreeAndNil(tempMOS);

  // Записываем общую длину
  Data :=  VTResult.GetNodeData(ParentNode);
  Data.Distance := totalDistance;
end;

{*******************************************************************************
* filterResultTree
*******************************************************************************}
procedure TFMain.filterResultTree(parentNode: PVirtualNode);
var
  node: PVirtualNode;
  nodeData: PResultNodeData;
begin
  node := parentNode.FirstChild;

  while (node <> nil) do
  begin
    nodeData := VTResult.GetNodeData(node);

    VTResult.IsVisible[node] := (nodeData.nodeType = rntRoute) or
                                (RBBranches.Checked) and (nodeData.nodeType = rntBranch) or
                                (RBBranches.Checked) and (nodeData.nodeType = rntBranchStreet) or
                                (RBStreets.Checked) and (nodeData.nodeType = rntStreet);

    filterResultTree(node);

    node := node.NextSibling;
  end;

  if (parentNode = VTResult.RootNode) then
  begin
    if (VTResult.SelectedCount = 0) or (not VTResult.IsVisible[VTResult.GetFirstSelected()]) then
    begin
      if (VTResult.VisibleCount > 0) then
      begin
        node := VTResult.GetFirstVisible();

        VTResult.Selected[node] := true;
        VTResult.Expanded[node] := true;
        VTResult.FocusedNode := node;
      end
    end;


    VTResult.Repaint();

  end;
end;

{**********************************************************************************************
* VTResultGetText
***********************************************************************************************}
procedure TFMain.VTResultGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PResultNodeData;
begin
  if (Node = nil) then
    exit;

  Data := VTResult.GetNodeData(Node);

  case Column of
    0:
      CellText := Data.Name;
    1:
      CellText := IntToStr(Ceil(Data.Distance)) + ' м.';
  end;
end;

{**********************************************************************************************
* RBBranchesClick
***********************************************************************************************}
procedure TFMain.RBBranchesClick(Sender: TObject);
begin
  filterResultTree(VTResult.RootNode);
end;

{**********************************************************************************************
* VTResultClick
***********************************************************************************************}
procedure TFMain.VTResultClick(Sender: TObject);
var
  Data : PResultNodeData;
  Node : PVirtualNode;
begin
  Node:=VTResult.FocusedNode;
  if not Assigned(Node) then
    Exit;

  Data := VTResult.GetNodeData (Node);
  FMap.Mappl.RefreshMapOverlay;

  If Data.nodeType = rntBranch Then
      FMap.Mappl.DrawMOS(ResultMosList[Data.okey], 3, 1, clYellow, clYellow);


end;

{**********************************************************************************************
* VTResultChange
***********************************************************************************************}
procedure TFMain.VTResultChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data : PResultNodeData;
begin
  Data := VTResult.GetNodeData(Node);

  if Data = nil then
    Exit;

  If Data.nodeType = rntRoute Then
    FMap.Mappl.FitToScreenByMOSList(ResultMosList);


end;

end.
