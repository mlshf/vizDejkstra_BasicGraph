unit AliasSettings;

interface

uses
  Classes, SysUtils, IniFiles, Forms, Windows, mapsSys;

type
  TIniAliasOptions = class(TObject)
  private
    FAliasList: TMapStrings;

    procedure LoadSettings(Ini: TIniFile);

  public
    constructor Create();
    destructor Destroy(); override;
    
    procedure LoadFromFile(const FileName: string);

    property Aliases: TMapStrings read FAliasList;
  end;

implementation

{**********************************************************************************************
* Create
***********************************************************************************************}
constructor TIniAliasOptions.Create();
begin
  FAliasList := TMapStrings.Create();
end;

{**********************************************************************************************
* Destroy
***********************************************************************************************}
destructor TIniAliasOptions.Destroy();
begin
  inherited;

  FreeAndNil(FAliasList);
end;

{**********************************************************************************************
* LoadSettings
***********************************************************************************************}
procedure TIniAliasOptions.LoadSettings(Ini: TIniFile);
var
  sections, keys: TStrings;
  i, j: integer;
  value: string;
begin
  FAliasList.Clear();

  if (Ini = nil) then
    exit;

  sections := TStringList.Create();
  keys := TStringList.Create();

  Ini.ReadSections(sections);

  for i := 0 to sections.Count - 1 do
  begin
    keys.Clear();
    Ini.ReadSection(sections[i], keys);

    for j := 0 to keys.Count - 1 do
    begin
      value := Ini.ReadString(sections[i], keys[j], '');

      if (value <> '') then
        FAliasList.addItem(keys[j], value);
    end;
  end;

  FreeAndNil(keys);
  FreeAndNil(sections);
end;

{**********************************************************************************************
* LoadFromFile
***********************************************************************************************}
procedure TIniAliasOptions.LoadFromFile(const FileName: string);
var
  Ini: TIniFile;
begin
  if FileExists(FileName) then
  begin
    Ini := TIniFile.Create(FileName);
    try
      LoadSettings(Ini);
    finally
      Ini.Free;
    end;
  end;
end;

end.

