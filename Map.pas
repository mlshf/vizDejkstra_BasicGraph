unit Map;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mappl, MapplTypes, MapObject, uniTypes, uniBaseTypes, uniUtils, Animation;

const
  START_STYLE_ID = 45604546117934367;
  END_STYLE_ID = 143246542004894475;

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

  TArrayEdge = Array of TEdge;

  TFMap = class(TForm)
    FMappl: TMappl;
    procedure FMapplMapMouseDown(Sender: TObject; aMapMode: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer; xMap,
      yMap: Double; var flAction: Boolean);
  private
    FTempObjects   : TMapplTempObjects;

    FStartNodeMode : Integer;
    FEndNodeMode   : Integer;

    FXFrom         : Double;
    FYFrom         : Double;
    FFromIndex     : Integer;

    FXTo           : Double;
    FYTo           : Double;
    FToIndex       : Integer;

    FStartX        : Double;
    FStartY        : Double;
    FOnSetPoint    : TNotifyEvent;
    FFieldList     : TStrings;
    FEdgesList     : TStrings;

  protected
    procedure DoSetPoint; dynamic;

  public
    property Mappl: TMappl read FMappl;
    property TempObjects: TMapplTempObjects read FTempObjects write FTempObjects;
    property StartNodeMode: Integer read FStartNodeMode write FStartNodeMode;
    property EndNodeMode: Integer read FEndNodeMode write FEndNodeMode;

    property XFrom: Double read FXFrom write FXFrom;
    property YFrom: Double read FYFrom write FYFrom;
    property FromIndex : Integer read FFromIndex write FFromIndex;

    property XTo: Double read FXTo write FXTo;
    property YTo: Double read FYTo write FYTo;
    property ToIndex : Integer read FToIndex write FToIndex;

    property StartX: Double read FStartX write FStartX;
    property StartY: Double read FStartY write FStartY;
    property OnSetPoint : TNotifyEvent read FOnSetPoint write FOnSetPoint;
    property FieldList  : TStrings read FFieldList write FFieldList;
    procedure GetEdgesByNodeIn(NodeIn: Integer; StartX, StartY : Double; var ResultArray : TArrayEdge; var ResultAmount : Integer);
    procedure SetStartPoint (aCenterX, aCenterY : Double);
    procedure SetEndPoint (aCenterX, aCenterY : Double);
    procedure DeleteStartPoint();
    procedure DeleteEndPoint();
  end;



implementation

uses MapObjectBase;

{$R *.dfm}

procedure TFMap.FMapplMapMouseDown(Sender: TObject; aMapMode: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; xMap,
  yMap: Double; var flAction: Boolean);

begin
  If (aMapMode <> FStartNodeMode) and (aMapMode <> FEndNodeMode) or (ssCtrl in Shift) Then
    Exit;

  If aMapMode = FStartNodeMode Then
  begin
    FXFrom := xMap;
    FYFrom := yMap;
    if FFromIndex <> -1 Then
      TempObjects.DelObject(FFromIndex);

    FFromIndex := TempObjects.AddPointObject(xMap, yMap, 15, dwoPointMss, START_STYLE_ID);

  end
  Else
  begin
    FXTo := xMap;
    FYTo := yMap;
    if FToIndex <> -1 Then
      TempObjects.DelObject(FToIndex);

    FToIndex := TempObjects.AddPointObject(xMap, yMap, 15, dwoPointMss, END_STYLE_ID);
  end;
  FMappl.RefreshMapOverlay;

  //Кидаем событие если вершина была найдена
  DoSetPoint();

end;

procedure TFMap.DoSetPoint;
begin
  if Assigned(FOnSetPoint) then FOnSetPoint(Self);
end;


{**********************************************************************************************
* GetEdgesByNodeIn
***********************************************************************************************}
procedure TFMap.GetEdgesByNodeIn(NodeIn : Integer; StartX, StartY : Double; var ResultArray : TArrayEdge; var ResultAmount : Integer);
var MOSList : TMapObjectStructureList;
    oAmount, i : Integer;

begin
  FEdgesList := TStringList.Create();
  MOSList := TMapObjectStructureList.Create(true);
  FEdgesList.Add('OKEY');
  FEdgesList.Add('NodeIn');
  FEdgesList.Add('NodeOut');
  ResultAmount := 0;
  SetLength(ResultArray, 0);

  oAmount := Mappl.GetLayerObjectsInMapPointToMosList(13,StartX,StartY,MOSList,10,ofmpDefault,false,true,'',true,FEdgesList,true);
  For i := 0 to oAmount-1 do
  begin
    If StrToInt(MOSList.Items[i].Value[1]) = NodeIn Then
    begin
      Inc(ResultAmount);
      SetLength(ResultArray, ResultAmount);
      ResultArray[ResultAmount - 1] := TEdge.Create();
      ResultArray[ResultAmount - 1].NodeIn := NodeIn;
      ResultArray[ResultAmount - 1].NodeOut := StrToInt(MOSList.Items[i].Value[2]);
      ResultArray[ResultAmount - 1].MOS := MOSList.Items[i];
    end;
  end;

  FreeAndNil(MOSList);
  FreeAndNil(FEdgesList);
end;

{**********************************************************************************************
* SetStartPoint
***********************************************************************************************}
procedure TFMap.SetStartPoint (aCenterX, aCenterY : Double);
begin
  If FFromIndex <> -1 Then
    TempObjects.DelObject(FFromIndex);

  FFromIndex := TempObjects.AddPointObject(aCenterX, aCenterY, 15, dwoPointMss, START_STYLE_ID);
  FXFrom := aCenterX;
  FYFrom := aCenterY;
  //TempObjects[FFromIndex].flSelected := true;
end;

{**********************************************************************************************
* SetEndPoint
***********************************************************************************************}
procedure TFMap.SetEndPoint (aCenterX, aCenterY : Double);
begin
  If FToIndex <> -1 Then
    TempObjects.DelObject(FToIndex);

  FToIndex := TempObjects.AddPointObject(aCenterX, aCenterY, 15, dwoPointMss, END_STYLE_ID);
  FXTo := aCenterX;
  FYTo := aCenterY;

  //TempObjects[FToIndex].flSelected := true;
end;

{**********************************************************************************************
* DeleteStartPoint
***********************************************************************************************}
procedure TFmap.DeleteStartPoint();
begin
  If FFromIndex <> -1 Then
  begin
    TempObjects.DelObject(FFromIndex);
    FFromIndex := -1;
    FMappl.RefreshMapOverlay;
  end;
end;

{**********************************************************************************************
* DeleteEndPoint
***********************************************************************************************}
procedure TFMap.DeleteEndPoint();
begin
  If FToIndex <> -1 Then
  begin
    TempObjects.DelObject(FToIndex);
    FToIndex := -1;
    FMappl.RefreshMapOverlay;
  end;
end;


end.
