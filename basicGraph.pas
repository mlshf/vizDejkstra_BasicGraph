unit basicGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes;

type
  EBasicGraphError = class(Exception);

  TBasicGraphDirection = ( bgdFrom = 0, bgdTo = 1 );

  TBasicGraphDirectionArray = set of TBasicGraphDirection;

  TBasicGraphDeletionMode = ( bgdmAllowOnlyTarget = 0, bgdmAllowTargetAndEdges = 1 );

  TBasicGraphEdge = class;

  TEdgeStruct = class
  private
    FEdge: TBasicGraphEdge;
    FCost: Double;
  public

    constructor Create( AEdge: TBasicGraphEdge; ACost: Double ); reintroduce;

    property Edge: TBasicGraphEdge read FEdge write FEdge;
    property Cost: Double read FCost write FCost;
  end;

  TEdgesQueueWithPriority = class
  private
    FQueue: TList;
  public
    constructor Create();
    function IsEmpty(): Boolean;
    function GetElement(): TEdgeStruct;
    //�������� ������� � ������������ ������
    procedure AddElement( AEdgeRecord: TEdgeStruct );
    destructor Destroy; override;
  end;

  //������� ����� - �������� ���� �����
  TBasicGraphNode = class( TObject )
  private
    FIndex: Integer;
    //���������� �������������
    FUID: Int64;
    //��������������� � ����� ������
    FObject: TObject;
    //���� �������� ������� ��� �������� ����
    FflOwnsObject: Boolean;
    //������ ���, �������� � ����
    FEdgesList_To: TList;
    //������ ���, ��������� �� ����
    FEdgesList_From: TList;
    //������ �����-�������, �� ������� ����� ������� � ����
    FNeighboursList_To: TList;
    //������ �����-�������, � ������� ����� ������� �� ����
    FNeighboursList_From: TList;

    //�������� ���� � ������ (��� � ��� ������) ���
    procedure AddEdge( AEdge: TBasicGraphEdge );

    //��������������� ��������� ��� �������� ���� � ��������� � ��� �������
    //������� �������� ���� � � ���� From, ���� �����
    procedure DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
    //������� �s������� ���� � � ���� To, ���� �����
    procedure DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
    //������� ���� �� ����� ������� ���
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //������� ���� �� ����� ������� ���, ����� � �� UID
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //������� ���� �� ����� ������� ���, ����� � �� �������
    procedure DeleteEdge_ByObject( AObject: TObject );

    //������ ����-������ � ������ ������� To/From �� UID
    function IndexOfNeighbour_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
    //������ ����-������ � ������ ������� To/From �� �������
    function IndexOfNeighbour_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;

  public
    constructor Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //�������� ������� ����-������
    //��������� ������� ����-������ � ������� ������� From/To/From&To
    function HasNeighbour( ANode: TBasicGraphNode; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //��������� ������� ����-������ � ������� ������� From/To/From&To �� UID
    function HasNeighbour_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //��������� ������� ����-������ � ������� ������� From/To/From&To �� ���������������� �������
    function HasNeighbour_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;

    //������ � ������-��������
    //���������� ���������� �����-������� � ������ From/To
    function GetNeighboursCount( AListDirection: TBasicGraphDirection ): Integer;
    //���������� ���� �� � ������� � ������ From/To
    function GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
    //���������� ���� �� � UID, ���� � ������� From/To/From&To
    function GetNeighbour_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
    //���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
    function GetNeighbour_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TBasicGraphNode;

    //----------------------------------------------------------------------------------------------------------------

    //������ ���� � ������ ��� To/From �� UID
    function IndexOfEdge_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
    //������ ���� � ������ ��� To/From �� �������
    function IndexOfEdge_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;

    //�������� ������� ����
    //��������� ������� ���� � ������� ��� From/To/From&To
    function HasEdge( AEdge: TBasicGraphEdge; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //��������� ������� ���� � ������� ��� From/To/From&To �� UID
    function HasEdge_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
    //��������� ������� ���� � ������� ��� From/To/From&To �� ���������������� �������
    function HasEdge_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;

    //������ � ������
    //���������� ���������� ��� � ������ From/To
    function GetEdgesCount( AListDirection: TBasicGraphDirection ): Integer;
    //���������� ���� �� � ������� � ������ From/To
    function GetEdge_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
    //���������� ���� �� � UID, ���� � ������� From/To/From&To
    function GetEdge_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
    //���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
    function GetEdge_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;

    //----------------------------------------------------------------------------------------------------------------

    //�������
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeIndex: Integer read FIndex;
  end;

  //������� ����� - �������� ���� �����
  TBasicGraphEdge = class( TObject )
  private
    FIndex: Integer;
    //���������� �������������
    FUID: Int64;
    //��������� � ����� ������
    FObject: TObject;
    //���� �������� ������� ��� �������� ����
    FflOwnsObject: Boolean;
    //���� - ������
    FNodeFrom: TBasicGraphNode;
    //���� - �����
    FNodeTo: TBasicGraphNode;
    //���� ����������������� ����. False (��-���������): ����������� ���� From -> To. True: ����������� ���� From <-> To. 
    FflBiDirected: Boolean;
    //��� ����. ��-��������� ����� 1.
    FWeight: Double;

  public
    constructor Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0 ); reintroduce;

    destructor Destroy(); override;

    //�������                    
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );
    procedure SetNodeFrom( ANewNodeFrom: TBasicGraphNode );
    procedure SetNodeTo( ANewNodeTo: TBasicGraphNode );
    procedure SetflBiDirected( AflBiDirected: Boolean );
    procedure SetWeight( AWeight: double );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property FlOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeFrom: TBasicGraphNode read FNodeFrom write SetNodeFrom;
    property NodeTo: TBasicGraphNode read FNodeTo write SetNodeTo;
    property FlBiDirected: Boolean read FflBiDirected write FflBiDirected;
    property Weight: Double read FWeight write SetWeight;
    property EdgeIndex: Integer read FIndex;
  end;

  TBasicGraph = class( TObject )
  private
    FHighestNodeIndexValue: Integer;
    FHIghestEdgeIndexValue: Integer;
    //���-���������, �������� ���� �� UID. �������� ������ ���������.
    FNodes_ByUID: THashContainer;
    //���-���������, �������� ���� �� ���������������� �������. �� �������� ������ ���������.
    FNodes_ByObject: THashContainer;
    //���-���������, �������� ���� �� UID. �������� ������ ���������.
    FEdges_ByUID: THashContainer;
    //���-���������, �������� ���� �� ���������������� �������. �������� ������ ���������.
    FEdges_ByObject: THashContainer;

  public
    constructor Create(); reintroduce;

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //���������� ����
    //�������� ���� � ������ �����, ���������� ������
    function AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;

    //�������� ����
    //������� ����. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode( ANode: TBasicGraphNode;
                          ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //������� ���� �� UID. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode_ByUID( AUID: Int64;
                                    ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
    //������� ���� �� �������. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode_ByObject( AObject: TObject;
                                  ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;

    //����� ����
    //�������� ���� �� ��� UID
    function GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
    //��������� ���� �� ��� �������
    function GetNode_ByObject( AObject: TOBject ): TBasicGraphNode; 

    //----------------------------------------------------------------------------------------------------------------

    //���������� ����
    //�������� ���� � ������ ���, ���������� ������
    function AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AWeight: Double = 1.0;
                      AObject: TObject = nil; AflOwnsObject: Boolean = false;
                      AflBiDirected: Boolean = false ): TBasicGraphEdge;

    //�������� ����
    //������� ����.
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //������� ���� �� UID.
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //������� ���� �� �������.
    procedure DeleteEdge_ByObject( AObject: TObject );

    //����� ����
    //�������� ���� �� � UID
    function GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
    //��������� ���� �� � �������
    function GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;

    //------------------------------------------------------------------------------------------------------------------

    function FindPath(  nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
  end;

implementation

constructor TEdgeStruct.Create( AEdge: TBasicGraphEdge; ACost: Double );
begin
  FEdge := AEdge;
  FCost := ACost;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Create
***********************************************************************************************}
constructor TEdgesQueueWithPriority.Create();
begin
  FQueue := TList.Create();
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.IsEmpty
***********************************************************************************************}
function TEdgesQueueWithPriority.IsEmpty(): Boolean;
begin
  Result := false;
  if ( FQueue.Count = 0 ) then
  begin
    Result := true;
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.GetElement
***********************************************************************************************}
function TEdgesQueueWithPriority.GetElement(): TEdgeStruct;
begin
  Result := nil;
  if FQueue.Count > 0 then
  begin
    Result := FQueue[ 0 ];
    FQueue.Delete( 0 );
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.AddElement
* �������� ������� � ������������ ������
***********************************************************************************************}
procedure TEdgesQueueWithPriority.AddElement( AEdgeRecord: TEdgeStruct );
var
  startIndex, endIndex, position: integer;
  edgeRecord: TEdgeStruct;
begin
  if ( FQueue.Count = 0 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  // ���� ����� ���������
  edgeRecord:= FQueue[0];
  if ( edgeRecord.Cost >= AEdgeRecord.Cost ) then
  begin
    FQueue.Insert( 0, AEdgeRecord );
    Exit;
  end;

  // ���� 1 ������� � ������
  if ( FQueue.Count = 1 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  startIndex := 0;
  endIndex := FQueue.Count - 1;
  while ( ( endIndex - startIndex ) > 1 ) do
  begin
    position := ( endIndex + startIndex ) div 2; //<---
    edgeRecord := FQueue[ position ];
    if ( edgeRecord.Cost = AEdgeRecord.Cost ) then
    begin
      FQueue.Insert( position, AEdgeRecord );
      Exit;
    end;

    if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
      endIndex:= position;
    if ( edgeRecord.Cost < AEdgeRecord.Cost ) then
      startIndex:= position;
  end;
  edgeRecord:= FQueue[ startIndex ];
  if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
    FQueue.Insert( startIndex, AEdgeRecord )
  else
    FQueue.Insert( endIndex, AEdgeRecord );
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Destroy
***********************************************************************************************}
destructor TEdgesQueueWithPriority.Destroy();
begin
  FreeAndNil( FQueue );
end;

{**********************************************************************************************
* TBasicGraphNode.SetObject
***********************************************************************************************}
procedure TBasicGraphNode.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphNode.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphNode.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphNode.Create
***********************************************************************************************}
constructor TBasicGraphNode.Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );
begin
  inherited Create();

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;

  FEdgesList_To := TList.Create();
  FEdgesList_From := TList.Create();
  FNeighboursList_To := TList.Create();
  FNeighboursList_From := TList.Create();
end;

{**********************************************************************************************
* TBasicGraphNode.Destroy
***********************************************************************************************}
destructor TBasicGraphNode.Destroy();
begin
  FreeAndNil( FEdgesList_To );
  FreeAndNil( FEdgesList_From );
  FreeAndNil( FNeighboursList_To );
  FreeAndNil( FNeighboursList_From );

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraphNode.AddEdge
* ��������� ���� � ��������������� ������ ���
* ��������� ����-����� � ��������������� ������ �������
***********************************************************************************************}
procedure TBasicGraphNode.AddEdge( AEdge: TBasicGraphEdge );

  //��������� ������ ������ � ������ From
  procedure AddEdge_FromSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //���� ���� ���� � ������ Edges From, �� ������ �����
    for index := 0 to FEdgesList_From.Count - 1 do
      if TBasicGraphEdge( FEdgesList_From[ index ] ) = AEdge then
        Exit;

    FEdgesList_From.Add( AEdge );

    //���� ���� ����� � ������ Neighbors From, �� ������ �����
    for index := 0 to FNeighboursList_From.Count - 1 do
      if TBasicGraphNode( FNeighboursList_From[ index ] ) = AEdge.NodeTo then
        Exit;
        
    FNeighboursList_From.Add( AEdge.NodeTo );
  end;

  //��������� ������ ������ � ������ To
  procedure AddEdge_ToSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //���� ���� ���� � ������ Edges To, �� ������ �����
    for index := 0 to FEdgesList_To.Count - 1 do
      if TBasicGraphEdge( FEdgesList_To[ index ] ) = AEdge then
        Exit;
    FEdgesList_To.Add( AEdge );

    //���� ���� ����� � ������ Neighbours To, �� ������ �����
    for index := 0 to FNeighboursList_To.Count - 1 do
      if TBasicGraphNode( FNeighboursList_To[ index ] ) = AEdge.NodeFrom then
        Exit;
    FNeighboursList_To.Add( AEdge.NodeFrom );
  end;

begin
  if AEdge = nil then
    raise EBasicGraphError.Create( '���������� �������� ���� � ������ ��� ����, �.�. ���� ������� (nil).' );

  if ( FEdgesList_To = nil )
    OR ( FEdgesList_From = nil )
    OR ( FNeighboursList_To = nil )
    OR ( FNeighboursList_From = nil ) then
    raise EBasicGraphError.Create( '���� �� ������� ���/������� ������� (nil).' );

  try
    //���� ���� ���������������
    if AEdge.flBiDirected then
    begin
      AddEdge_FromSelf( AEdge );
      AddEdge_ToSelf( AEdge );
    end
    else//���� ����������������
    begin
      if Self = AEdge.NodeFrom then
      begin
        AddEdge_FromSelf( AEdge );
      end
      else
      begin
        if Self = AEdge.NodeTo then
        begin
          AddEdge_ToSelf( AEdge );  
        end
        else
          raise EBasicGraphError.Create( '������� �������� � ������ ��� ���� ����, �� ��������� � ���� �����!' );
      end;
    
    end;
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� �������� ���� � ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;

  FEdgesList_To.Delete(index);
  flDeleteNode := true;

  //������ ���� ���������, �������� �� ���� ������� ����, �� �������� �������� ����
  for index := 0 to FEdgesList_To.Count - 1 do
    if TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom = NodeFrom then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_To.IndexOf( NodeFrom );
    if index >= 0 then
      FNeighboursList_To.Delete( index );
  end;
end;

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;
    
  FEdgesList_From.Delete(index);
  flDeleteNode := true;

  //������ ���� ���������, �������� �� ���� ������� ����, � ������� ������� ����
  for index := 0 to FEdgesList_From.Count - 1 do
    if TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo = NodeTo then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_From.IndexOf( NodeTo );
    if index >= 0 then
      FNeighboursList_From.Delete( index );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge( AEdge: TBasicGraphEdge );
var
  index: Integer;
begin
  if AEdge = nil then
    raise EBasicGraphError.Create( '���������� ������� ����, ��������� ��� ������� (nil).' );

  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := FEdgesList_To.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, AEdge.NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := FEdgesList_From.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, AEdge.NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByUID
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByUID( AUID: Int64 );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
    
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := IndexOfEdge_ByUID( bgdTo, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := IndexOfEdge_ByUID( bgdFrom, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByObject
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByObject( AObject: TObject );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
    
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := IndexOfEdge_ByObject( bgdTo, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := IndexOfEdge_ByObject( bgdFrom, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour
* ��������� ������� ����-������ � ������� ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour( ANode: TBasicGraphNode; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( FNeighboursList_To.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( FNeighboursList_From.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = bgdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = bgdFrom then
    aNeighboursList := FNeighboursList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = bgdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = bgdFrom then
    aNeighboursList := FNeighboursList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByUID
*��������� ������� ����-������ � ������� ������� From/To/From&To �� UID
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfNeighbour_ByUID( bgdTo, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByUID( bgdFrom, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByObject
* ��������� ������� ����-������ � ������� ������� From/To/From&To �� ���������������� �������
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
    raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfNeighbour_ByObject( bgdTo, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByObject( bgdFrom, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighboursCount
* ���������� ���������� �����-������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighboursCount( AListDirection: TBasicGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = bgdTo then
    Result := FNeighboursList_To.Count
  else if AListDirection = bgdFrom then
    Result := FNeighboursList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByIndex
* ���������� ���� �� � ������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphNode;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = bgdTo )
      AND ( AIndex >= FNeighboursList_To.Count ) )
  OR ( ( AListDirection = bgdFrom )
      AND ( AIndex >= FNeighboursList_From.Count ) ) then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ AIndex ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByUID
* ���������� ���� �� � UID, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByObject   
* ���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge
* ��������� ������� ���� � ������� ��� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasEdge( AEdge: TBasicGraphEdge; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( FEdgesList_To.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( FEdgesList_From.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByUID( AListDirection: TBasicGraphDirection; AUID: Int64 ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = bgdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = bgdFrom then
    aEdgesList := FEdgesList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByObject( AListDirection: TBasicGraphDirection; AObject: TObject ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = bgdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = bgdFrom then
    aEdgesList := FEdgesList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByUID
* ��������� ������� ���� � ������� ��� From/To/From&To �� UID
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByUID( AUID: Int64; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfEdge_ByUID( bgdTo, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfEdge_ByUID( bgdFrom, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByObject
* ��������� ������� ���� � ������� ��� From/To/From&To �� ���������������� �������
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByObject( AObject: TObject; AListDirections: TBasicGraphDirectionArray ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EBasicGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( bgdTo in AListDirections )
  AND ( IndexOfEdge_ByObject( bgdTo, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( bgdFrom in AListDirections )
  AND ( IndexOfEdge_ByObject( bgdFrom, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdgesCount
* ���������� ���������� ��� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdgesCount( AListDirection: TBasicGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = bgdTo then
    Result := FEdgesList_To.Count
  else if AListDirection = bgdFrom then
    Result := FEdgesList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByIndex
* ���������� ���� �� � ������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByIndex( AIndex: Integer; AListDirection: TBasicGraphDirection ): TBasicGraphEdge;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = bgdTo )
      AND ( AIndex >= FEdgesList_To.Count ) )
  OR ( ( AListDirection = bgdFrom )
      AND ( AIndex >= FEdgesList_From.Count ) ) then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ AIndex ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByUID
* ���������� ���� �� � UID, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByUID( AUID: Int64; AListDirection: TBasicGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByObject
* ���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByObject( AObject: TObject; AListDirection: TBasicGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = bgdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = bgdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

//----------------------------------------------------------------------------------------------------------------------
//������ �������� ����
{**********************************************************************************************
* TBasicGraphEdge.SetObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  Self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeFrom( ANewNodeFrom: TBasicGraphNode );
begin
  Self.FNodeFrom := ANewNodeFrom;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeTo( ANewNodeTo: TBasicGraphNode );
begin
  Self.FNodeTo := ANewNodeTo;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetDirection
***********************************************************************************************}
procedure TBasicGraphEdge.SetflBiDirected( AflBiDirected: Boolean );
begin
  Self.FflBiDirected := AflBiDirected;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetWeight
***********************************************************************************************}
procedure TBasicGraphEdge.SetWeight( AWeight: double );
begin
  Self.FWeight := AWeight;
end;

{**********************************************************************************************
* TBasicGraphEdge.Create
***********************************************************************************************}
constructor TBasicGraphEdge.Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1;
                                    AObject: TObject = nil; AflOwnsObject: Boolean = false;
                                    AflBiDirected: Boolean = false; AWeight: Double = 1.0 );
begin
  inherited Create();

  if ( ANodeFrom = nil ) or ( ANodeTo = nil ) then
    raise EBasicGraphError.Create( '��� ���� ���� ������ ������������ (�� nil).' );

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;
  FNodeFrom := ANodeFrom;
  FNodeTo := ANodeTo;
  FWeight := AWeight;
  FflBiDirected := AflBiDirected;

  //����� AddEdge ��� ��������� ���� � ��� ���� �������� ����
  FNodeFrom.AddEdge( Self );
  FNodeTo.AddEdge( Self );
end;

{**********************************************************************************************
* TBasicGraphEdge.Destroy
***********************************************************************************************}
destructor TBasicGraphEdge.Destroy();
begin
  FNodeTo := nil;
  FNodeFrom := nil;

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
end;

//----------------------------------------------------------------------------------------------------------------------
//������ �������� �����
{**********************************************************************************************
* TBasicGraph.Create
***********************************************************************************************}
constructor TBasicGraph.Create();
begin
  inherited Create();

  FHighestNodeIndexValue := 0;
  FHIghestEdgeIndexValue := 0;

  FNodes_ByUID := THashContainer.Create( True );
  FNodes_ByObject := THashContainer.Create( );
  FEdges_ByUID := THashContainer.Create( True );
  FEdges_ByObject := THashContainer.Create( );
end;

destructor TBasicGraph.Destroy();
begin
  FreeAndNil( FNodes_ByUID );
  FreeAndNil( FNodes_ByObject );
  FreeAndNil( FEdges_ByUID );
  FreeAndNil( FEdges_ByObject );

  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraph.AddNode
* �������� ���� � ������ �����, ���������� ������
***********************************************************************************************}
function TBasicGraph.AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;
var
  node: TBasicGraphNode;
begin
  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EBasicGraphError.Create( '���������� �������� ���� � �������������� (nil) ������ �����.' );

  try
    node := TBasicGraphNode.Create( AUID, FHighestNodeIndexValue, AObject, AflOwnsObject );
    FHighestNodeIndexValue := FHighestNodeIndexValue + 1;

    FNodes_ByUID.AddObject( node.UID, node, true );
    FNodes_ByObject.AddObject( Integer( node.Object_ ), node, true );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� �������� ����. ��������� ������:' + sLineBreak + E.Message );
  end;

  Result := node;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode
***********************************************************************************************}
function TBasicGraph.DeleteNode( ANode: TBasicGraphNode;
                                 ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  if ( ANode.GetEdgesCount( bgdTo ) + ANode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByUID
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByUID( AUID: Int64;
                                       ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  aNode := GetNode_ByUID( AUID );

  if ( aNode.GetEdgesCount( bgdTo ) + aNode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByObject
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByObject( AObject: TObject;
                                       ADeletionMode: TBasicGraphDeletionMode = bgdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  aNode := GetNode_ByObject( AObject );

  if ( aNode.GetEdgesCount( bgdTo ) + aNode.GetEdgesCount( bgdFrom ) > 0 )
  and ( ADeletionMode = bgdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( bgdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( bgdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, bgdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetNode_ByUID
* �������� ���� �� ��� UID
***********************************************************************************************}
function TBasicGraph.GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
begin
  Result := TBasicGraphNode( FNodes_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.FindNode_ByUniqueID
***********************************************************************************************}
function TBasicGraph.GetNode_ByObject( AObject: TObject ): TBasicGraphNode;
begin
  Result := TbasicGraphNode( FNodes_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
*  TBasicGraph.AddEdge
***********************************************************************************************}
function TBasicGraph.AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AWeight: Double = 1.0;
                      AObject: TObject = nil; AflOwnsObject: Boolean = false;
                      AflBiDirected: Boolean = false ): TBasicGraphEdge;
var
  edge: TBasicGraphEdge;
begin
  if ( FEdges_ByUID = nil )
  OR ( FEdges_ByObject = nil) then
    raise EBasicGraphError.Create( '���������� �������� ���� � �������������� (nil) ������ ���.' );

  try
    edge := TBasicGraphEdge.Create( ANodeFrom, ANodeTo, AUID, FHighestEdgeIndexValue, AObject, AflOwnsObject, AflBiDirected, AWeight );
    FHighestEdgeIndexValue := FHighestEdgeIndexValue + 1;

    FEdges_ByUID.AddObject( edge.UID, edge, true );
    FEdges_ByObject.AddObject( Integer( edge.Object_ ), edge, true );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� �������� ����. ��������� ������:' + sLineBreak + E.Message );
  end;

  Result := edge;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge
* ������� ���� �� ������ ���
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge( AEdge: TBasicGraphEdge );
begin
  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    AEdge.NodeTo.DeleteEdge( AEdge );
    AEdge.NodeFrom.DeleteEdge( AEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( AEdge.UID, AEdge );
    FEdges_ByObject.DelObject( Integer( AEdge.Object_ ), AEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByUID
* ������� ���� �� ������, ����� �� Unique ID
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByUID( AUID: Int64 );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByUID( AUID );

  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByObject
* ������� ���� �� �������
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByObject( AObject: TObject );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByObject( AObject );

  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByUID
* �������� ���� �� � UID
***********************************************************************************************}
function TBasicGraph.GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByObject
* ��������� ���� �� � �������
***********************************************************************************************}
function TBasicGraph.GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
* TBasicGraph.FindPath
***********************************************************************************************}
function TBasicGraph.FindPath( nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
var
  distanceFromSource: array of Double;
  incomingEdge: array of TBasicGraphEdge;
  queuedEdges: TEdgesQueueWithPriority;
  index: integer;
  currentNode: TBasicGraphNode;
  queuedEdge, currentEdge: TBasicGraphEdge;
  edgeStruct: TEdgeStruct;
  currentDistanceFromSource: Double;

  procedure AddStartEdgesToArray( startNode: TBasicGraphNode );
  var
    index: Integer;
    edge: TBasicGraphEdge;
  begin
    for index := 0 to startNode.GetEdgesCount( bgdFrom ) - 1 do
    begin
      edge := startNode.GetEdge_ByIndex( index, bgdFrom );
      distanceFromSource[ edge.NodeTo.NodeIndex ] := edge.Weight;
      incomingEdge[ edge.NodeTo.NodeIndex ] := edge;
      queuedEdges.AddElement( TEdgeStruct.Create( edge, edge.Weight ) );
    end;
  end;

begin
  if nodeFrom = nodeTo then
  begin
    Result := 0;
    exit;
  end;

  Result := -1;
  try
    SetLength( distanceFromSource, FNodes_ByUID.ObjectCount );
    SetLength( incomingEdge, FNodes_ByUID.ObjectCount );
    queuedEdges := TEdgesQueueWithPriority.Create();
    //-------------------------------------------------------------------------------------------------------------------
    //�������������
    for index := 0 to FNodes_ByUID.ObjectCount - 1 do
    begin
      distanceFromSource[ index ] := -1.0;
      incomingEdge[ index ] := nil;
    end;

    //���������� �� Source �� Source = 0
    //������ �������������, �� ������ ����� ������ �������, ��������� � Source - ������ ��� �� ���
    distanceFromSource[ nodeFrom.NodeIndex ] := 0;
    incomingEdge[ nodeFrom.NodeIndex ] := nil;
    AddStartEdgesToArray( nodeFrom );

    //-------------------------------------------------------------------------------------------------------------------
    //������ ��� ���������
    while ( not queuedEdges.IsEmpty() ) do
    begin
      edgeStruct := queuedEdges.GetElement();
      if ( edgeStruct.Cost <> distanceFromSource[ edgeStruct.Edge.NodeTo.NodeIndex ] ) then
      begin
        edgeStruct.Edge := nil;
        FreeAndNil( edgeStruct );
        continue;
      end;

      queuedEdge := edgeStruct.Edge;
      currentNode := queuedEdge.NodeTo;
      for index := 0 to currentNode.GetEdgesCount( bgdFrom ) - 1 do
      begin
        currentEdge := currentNode.GetEdge_ByIndex( index, bgdFrom );
        currentDistanceFromSource := distanceFromSource[ currentNode.NodeIndex ] + currentEdge.Weight;
        if ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] < 0 )
          OR ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] > currentDistanceFromSource ) then
        begin
          distanceFromSource[ currentEdge.NodeTo.NodeIndex ] := currentDistanceFromSource;
          incomingEdge[ currentEdge.NodeTo.NodeIndex ] := currentEdge;
          queuedEdges.AddElement( TEdgeStruct.Create( currentEdge, currentDistanceFromSource ) );
        end;
      end;    
      FreeAndNil( edgeStruct );
    end;
    FreeAndNil( queuedEdges );

    //-------------------------------------------------------------------------------------------------------------------
    //�������� ��� ���������
    currentEdge := incomingEdge[ nodeTo.NodeIndex ];
    if currentEdge <> nil then
      Result := 0;
      
    while currentEdge <> nil do
    begin
      path.Insert( 0, currentEdge );
      Result := Result + currentEdge.Weight;
      currentEdge := incomingEdge[ currentEdge.NodeFrom.NodeIndex ];
    end;
  finally
    FreeAndNil( queuedEdges );
  end;
end;

end.
