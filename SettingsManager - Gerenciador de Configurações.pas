unit SettingsManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils, System.IniFiles,
  System.Generics.Collections, System.Variants, System.DateUtils, System.RegularExpressions;

type
  TSettingType = (stString, stInteger, stFloat, stBoolean, stArray, stObject);
  
  TSettingValidation = record
    Required: Boolean;
    MinValue: Variant;
    MaxValue: Variant;
    Pattern: string;
    AllowedValues: TArray<Variant>;
    CustomValidator: TFunc<Variant, Boolean>;
  end;
  
  TSetting = record
    Name: string;
    Value: Variant;
    DefaultValue: Variant;
    SettingType: TSettingType;
    Description: string;
    Category: string;
    Validation: TSettingValidation;
    IsAdvanced: Boolean;
    RequiresRestart: Boolean;
    LastModified: TDateTime;
  end;
  
  TSettingsCategory = record
    Name: string;
    DisplayName: string;
    Description: string;
    Icon: string;
    Order: Integer;
    Settings: TArray<TSetting>;
  end;
  
  TSettingsProfile = record
    Name: string;
    Description: string;
    Settings: TDictionary<string, Variant>;
    CreatedDate: TDateTime;
    IsDefault: Boolean;
    Author: string;
  end;
  
  TSettingsValidationResult = record
    IsValid: Boolean;
    Errors: TArray<string>;
    Warnings: TArray<string>;
  end;
  
  TSettingsChangeEvent = procedure(Sender: TObject; const SettingName: string; 
    const OldValue, NewValue: Variant) of object;
  TSettingsValidationEvent = procedure(Sender: TObject; const SettingName: string; 
    const Value: Variant; var IsValid: Boolean; var ErrorMessage: string) of object;
  TSettingsLoadEvent = procedure(Sender: TObject; Success: Boolean; const Message: string) of object;
  
  TSettingsManager = class
  private
    FSettings: TDictionary<string, TSetting>;
    FCategories: TList<TSettingsCategory>;
    FProfiles: TDictionary<string, TSettingsProfile>;
    FCurrentProfile: string;
    FConfigPath: string;
    FConfigFormat: string; // json, ini, xml
    FAutoSave: Boolean;
    FBackupSettings: Boolean;
    FMaxBackups: Integer;
    FEncryptSensitive: Boolean;
    FValidationEnabled: Boolean;
    
    // Eventos
    FOnSettingChanged: TSettingsChangeEvent;
    FOnValidation: TSettingsValidationEvent;
    FOnLoad: TSettingsLoadEvent;
    FOnSave: TSettingsLoadEvent;
    
    // Cache e performance
    FSettingsCache: TDictionary<string, Variant>;
    FCacheEnabled: Boolean;
    FLastSaveTime: TDateTime;
    FSaveInterval: Integer; // segundos
    FDirtySettings: TStringList;
    
    function GetConfigFileName: string;
    function GetBackupPath: string;
    function ValidateSetting(const Setting: TSetting; const Value: Variant): TSettingsValidationResult;
    function SerializeSettings: string;
    function DeserializeSettings(const Data: string): Boolean;
    function EncryptValue(const Value: string): string;
    function DecryptValue(const Value: string): string;
    function IsSettingSensitive(const SettingName: string): Boolean;
    procedure CreateBackup;
    procedure CleanupBackups;
    procedure InitializeDefaultSettings;
    procedure InitializeCategories;
    procedure LoadFromJSON(const JSONData: string);
    procedure LoadFromINI(const INIFile: TIniFile);
    procedure SaveToJSON(var JSONData: string);
    procedure SaveToINI(const INIFile: TIniFile);
    procedure NotifySettingChanged(const SettingName: string; const OldValue, NewValue: Variant);
    function ConvertVariantToType(const Value: Variant; TargetType: TSettingType): Variant;
    procedure UpdateCache(const SettingName: string; const Value: Variant);
    procedure InvalidateCache;
    
  public
    constructor Create(const ConfigPath: string = '');
    destructor Destroy; override;
    
    // Propriedades
    property ConfigPath: string read FConfigPath write FConfigPath;
    property ConfigFormat: string read FConfigFormat write FConfigFormat;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property BackupSettings: Boolean read FBackupSettings write FBackupSettings;
    property MaxBackups: Integer read FMaxBackups write FMaxBackups;
    property EncryptSensitive: Boolean read FEncryptSensitive write FEncryptSensitive;
    property ValidationEnabled: Boolean read FValidationEnabled write FValidationEnabled;
    property CurrentProfile: string read FCurrentProfile;
    property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
    property SaveInterval: Integer read FSaveInterval write FSaveInterval;
    
    // Eventos
    property OnSettingChanged: TSettingsChangeEvent read FOnSettingChanged write FOnSettingChanged;
    property OnValidation: TSettingsValidationEvent read FOnValidation write FOnValidation;
    property OnLoad: TSettingsLoadEvent read FOnLoad write FOnLoad;
    property OnSave: TSettingsLoadEvent read FOnSave write FOnSave;
    
    // Gerenciamento básico de configurações
    function LoadSettings: Boolean;
    function SaveSettings: Boolean;
    function ReloadSettings: Boolean;
    function HasUnsavedChanges: Boolean;
    function ResetToDefaults: Boolean;
    function ResetSetting(const SettingName: string): Boolean;
    
    // Getters e Setters
    function GetSetting(const SettingName: string): Variant; overload;
    function GetSetting(const SettingName: string; const DefaultValue: Variant): Variant; overload;
    function SetSetting(const SettingName: string; const Value: Variant): Boolean;
    function GetSettingAsString(const SettingName: string; const Default: string = ''): string;
    function GetSettingAsInteger(const SettingName: string; const Default: Integer = 0): Integer;
    function GetSettingAsFloat(const SettingName: string; const Default: Double = 0.0): Double;
    function GetSettingAsBoolean(const SettingName: string; const Default: Boolean = False): Boolean;
    function GetSettingAsArray(const SettingName: string): TArray<Variant>;
    function GetSettingAsObject(const SettingName: string): TJSONObject;
    
    function SetSettingString(const SettingName, Value: string): Boolean;
    function SetSettingInteger(const SettingName: string; const Value: Integer): Boolean;
    function SetSettingFloat(const SettingName: string; const Value: Double): Boolean;
    function SetSettingBoolean(const SettingName: string; const Value: Boolean): Boolean;
    function SetSettingArray(const SettingName: string; const Value: TArray<Variant>): Boolean;
    function SetSettingObject(const SettingName: string; const Value: TJSONObject): Boolean;
    
    // Gerenciamento de configurações
    function SettingExists(const SettingName: string): Boolean;
    function RegisterSetting(const Setting: TSetting): Boolean;
    function UnregisterSetting(const SettingName: string): Boolean;
    function GetSettingInfo(const SettingName: string): TSetting;
    function GetAllSettings: TArray<TSetting>;
    function GetSettingsByCategory(const Category: string): TArray<TSetting>;
    function GetAdvancedSettings: TArray<TSetting>;
    function GetRequiredSettings: TArray<TSetting>;
    
    // Validação
    function ValidateSettings: TSettingsValidationResult;
    function ValidateSettingValue(const SettingName: string; const Value: Variant): TSettingsValidationResult;
    function AddValidationRule(const SettingName: string; const Validation: TSettingValidation): Boolean;
    function RemoveValidationRule(const SettingName: string): Boolean;
    
    // Categorias
    function AddCategory(const Category: TSettingsCategory): Boolean;
    function RemoveCategory(const CategoryName: string): Boolean;
    function GetCategories: TArray<TSettingsCategory>;
    function GetCategory(const CategoryName: string): TSettingsCategory;
    function MoveSetting(const SettingName, NewCategory: string): Boolean;
    
    // Perfis
    function CreateProfile(const ProfileName, Description: string): Boolean;
    function DeleteProfile(const ProfileName: string): Boolean;
    function LoadProfile(const ProfileName: string): Boolean;
    function SaveProfile(const ProfileName: string): Boolean;
    function GetProfiles: TArray<TSettingsProfile>;
    function SetDefaultProfile(const ProfileName: string): Boolean;
    function ExportProfile(const ProfileName, FileName: string): Boolean;
    function ImportProfile(const FileName: string): Boolean;
    function CloneProfile(const SourceProfile, TargetProfile: string): Boolean;
    
    // Importação e Exportação
    function ExportSettings(const FileName: string; Format: string = 'json'): Boolean;
    function ImportSettings(const FileName: string; MergeMode: Boolean = False): Boolean;
    function ExportCategory(const CategoryName, FileName: string): Boolean;
    function ImportCategory(const FileName: string): Boolean;
    
    // Backup e Restauração
    function CreateBackupNow: string;
    function RestoreFromBackup(const BackupFile: string): Boolean;
    function GetBackupList: TArray<string>;
    function DeleteBackup(const BackupFile: string): Boolean;
    function GetBackupInfo(const BackupFile: string): TJSONObject;
    
    // Busca e Filtros
    function SearchSettings(const Query: string): TArray<TSetting>;
    function FilterSettings(const Filter: TFunc<TSetting, Boolean>): TArray<TSetting>;
    function GetModifiedSettings(Since: TDateTime): TArray<TSetting>;
    function GetSettingsRequiringRestart: TArray<TSetting>;
    
    // Sincronização e Monitoramento
    function StartAutoSave: Boolean;
    function StopAutoSave: Boolean;
    function ForceSave: Boolean;
    function WatchForChanges(const SettingNames: TArray<string>): Boolean;
    function StopWatching: Boolean;
    
    // Utilitários
    function GetConfigSummary: TJSONObject;
    function GetSettingsReport: string;
    function VerifyIntegrity: Boolean;
    function OptimizeSettings: Boolean;
    function GetSettingDependencies(const SettingName: string): TArray<string>;
    function CheckCompatibility(const Version: string): Boolean;
    function MigrateSettings(const FromVersion, ToVersion: string): Boolean;
    
    // Configurações específicas da aplicação
    procedure InitializeOllamaSettings;
    procedure InitializeTrainingSettings;
    procedure InitializeUISettings;
    procedure InitializeAdvancedSettings;
    procedure InitializeSecuritySettings;
  end;

implementation

uses
  System.NetEncoding, System.Hash, Winapi.Windows;

{ TSettingsManager }

constructor TSettingsManager.Create(const ConfigPath: string);
begin
  inherited Create;
  
  FSettings := TDictionary<string, TSetting>.Create;
  FCategories := TList<TSettingsCategory>.Create;
  FProfiles := TDictionary<string, TSettingsProfile>.Create;
  FSettingsCache := TDictionary<string, Variant>.Create;
  FDirtySettings := TStringList.Create;
  
  // Configurações padrão
  if ConfigPath <> '' then
    FConfigPath := ConfigPath
  else
    FConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'OllamaTrainer');
    
  FConfigFormat := 'json';
  FAutoSave := True;
  FBackupSettings := True;
  FMaxBackups := 10;
  FEncryptSensitive := True;
  FValidationEnabled := True;
  FCacheEnabled := True;
  FSaveInterval := 30; // 30 segundos
  FCurrentProfile := 'default';
  
  // Garantir que o diretório existe
  ForceDirectories(FConfigPath);
  
  // Inicializar configurações padrão
  InitializeDefaultSettings;
  InitializeCategories;
  
  // Carregar configurações existentes
  LoadSettings;
end;

destructor TSettingsManager.Destroy;
var
  Setting: TSetting;
  Profile: TSettingsProfile;
begin
  // Salvar se houver mudanças pendentes
  if FAutoSave and HasUnsavedChanges then
    SaveSettings;
  
  // Liberar recursos dos settings
  for var Pair in FSettings do
  begin
    Setting := Pair.Value;
    if Setting.SettingType = stArray then
    begin
      // Liberar arrays se necessário
    end
    else if Setting.SettingType = stObject then
    begin
      // Liberar objetos JSON se necessário
    end;
  end;
  
  // Liberar recursos dos perfis
  for var Pair in FProfiles do
  begin
    Profile := Pair.Value;
    Profile.Settings.Free;
  end;
  
  FDirtySettings.Free;
  FSettingsCache.Free;
  FProfiles.Free;
  FCategories.Free;
  FSettings.Free;
  
  inherited Destroy;
end;

function TSettingsManager.GetConfigFileName: string;
begin
  case LowerCase(FConfigFormat) of
    'json': Result := TPath.Combine(FConfigPath, 'settings.json');
    'ini': Result := TPath.Combine(FConfigPath, 'settings.ini');
    'xml': Result := TPath.Combine(FConfigPath, 'settings.xml');
  else
    Result := TPath.Combine(FConfigPath, 'settings.json');
  end;
end;

function TSettingsManager.LoadSettings: Boolean;
var
  ConfigFile: string;
  Content: string;
  IniFile: TIniFile;
begin
  Result := False;
  
  try
    ConfigFile := GetConfigFileName;
    
    if not FileExists(ConfigFile) then
    begin
      // Arquivo não existe, usar configurações padrão
      Result := True;
      if Assigned(FOnLoad) then
        FOnLoad(Self, True, 'Configurações padrão carregadas');
      Exit;
    end;
    
    case LowerCase(FConfigFormat) of
      'json':
      begin
        Content := TFile.ReadAllText(ConfigFile, TEncoding.UTF8);
        LoadFromJSON(Content);
      end;
      
      'ini':
      begin
        IniFile := TIniFile.Create(ConfigFile);
        try
          LoadFromINI(IniFile);
        finally
          IniFile.Free;
        end;
      end;
    end;
    
    FLastSaveTime := Now;
    FDirtySettings.Clear;
    InvalidateCache;
    
    Result := True;
    
    if Assigned(FOnLoad) then
      FOnLoad(Self, True, 'Configurações carregadas com sucesso');
      
  except
    on E: Exception do
    begin
      if Assigned(FOnLoad) then
        FOnLoad(Self, False, 'Erro ao carregar configurações: ' + E.Message);
    end;
  end;
end;

function TSettingsManager.SaveSettings: Boolean;
var
  ConfigFile: string;
  Content: string;
  IniFile: TIniFile;
  BackupCreated: Boolean;
begin
  Result := False;
  BackupCreated := False;
  
  try
    ConfigFile := GetConfigFileName;
    
    // Criar backup se habilitado
    if FBackupSettings and FileExists(ConfigFile) then
    begin
      CreateBackup;
      BackupCreated := True;
    end;
    
    case LowerCase(FConfigFormat) of
      'json':
      begin
        SaveToJSON(Content);
        TFile.WriteAllText(ConfigFile, Content, TEncoding.UTF8);
      end;
      
      'ini':
      begin
        IniFile := TIniFile.Create(ConfigFile);
        try
          SaveToINI(IniFile);
        finally
          IniFile.Free;
        end;
      end;
    end;
    
    FLastSaveTime := Now;
    FDirtySettings.Clear;
    
    Result := True;
    
    if Assigned(FOnSave) then
      FOnSave(Self, True, 'Configurações salvas com sucesso');
      
  except
    on E: Exception do
    begin
      if Assigned(FOnSave) then
        FOnSave(Self, False, 'Erro ao salvar configurações: ' + E.Message);
    end;
  end;
end;

procedure TSettingsManager.LoadFromJSON(const JSONData: string);
var
  JSONObj: TJSONObject;
  SettingsObj: TJSONObject;
  SettingName: string;
  SettingValue: TJSONValue;
  Setting: TSetting;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONData) as TJSONObject;
  if not Assigned(JSONObj) then
    raise Exception.Create('JSON inválido');
    
  try
    // Carregar configurações básicas
    if JSONObj.TryGetValue<TJSONObject>('settings', SettingsObj) then
    begin
      for var Pair in SettingsObj do
      begin
        SettingName := Pair.JsonString.Value;
        SettingValue := Pair.JsonValue;
        
        if FSettings.TryGetValue(SettingName, Setting) then
        begin
          case Setting.SettingType of
            stString: Setting.Value := SettingValue.Value;
            stInteger: Setting.Value := SettingValue.AsType<Integer>;
            stFloat: Setting.Value := SettingValue.AsType<Double>;
            stBoolean: Setting.Value := SettingValue.AsType<Boolean>;
            stArray: 
            begin
              var ArrayValue := SettingValue as TJSONArray;
              var Values: TArray<Variant>;
              SetLength(Values, ArrayValue.Count);
              for var I := 0 to ArrayValue.Count - 1 do
                Values[I] := ArrayValue.Items[I].Value;
              Setting.Value := Values;
            end;
            stObject: Setting.Value := SettingValue.ToJSON;
          end;
          
          FSettings.AddOrSetValue(SettingName, Setting);
        end;
      end;
    end;
    
    // Carregar perfis se existirem
    var ProfilesObj: TJSONObject;
    if JSONObj.TryGetValue<TJSONObject>('profiles', ProfilesObj) then
    begin
      for var Pair in ProfilesObj do
      begin
        var ProfileName := Pair.JsonString.Value;
        var ProfileData := Pair.JsonValue as TJSONObject;
        
        var Profile: TSettingsProfile;
        Profile.Name := ProfileName;
        Profile.Description := ProfileData.GetValue<string>('description', '');
        Profile.CreatedDate := ISO8601ToDate(ProfileData.GetValue<string>('created', ''));
        Profile.IsDefault := ProfileData.GetValue<Boolean>('is_default', False);
        Profile.Author := ProfileData.GetValue<string>('author', '');
        Profile.Settings := TDictionary<string, Variant>.Create;
        
        // Carregar configurações do perfil
        var ProfileSettingsObj: TJSONObject;
        if ProfileData.TryGetValue<TJSONObject>('settings', ProfileSettingsObj) then
        begin
          for var SettingPair in ProfileSettingsObj do
            Profile.Settings.Add(SettingPair.JsonString.Value, SettingPair.JsonValue.Value);
        end;
        
        FProfiles.AddOrSetValue(ProfileName, Profile);
      end;
    end;
    
    // Carregar perfil atual
    FCurrentProfile := JSONObj.GetValue<string>('current_profile', 'default');
    
  finally
    JSONObj.Free;
  end;
end;

procedure TSettingsManager.SaveToJSON(var JSONData: string);
var
  JSONObj: TJSONObject;
  SettingsObj: TJSONObject;
  ProfilesObj: TJSONObject;
  Setting: TSetting;
  Profile: TSettingsProfile;
begin
  JSONObj := TJSONObject.Create;
  try
    // Metadados
    JSONObj.AddPair('version', '1.0');
    JSONObj.AddPair('saved_at', DateToISO8601(Now));
    JSONObj.AddPair('current_profile', FCurrentProfile);
    
    // Configurações
    SettingsObj := TJSONObject.Create;
    for var Pair in FSettings do
    begin
      Setting := Pair.Value;
      
      case Setting.SettingType of
        stString: 
        begin
          if IsSettingSensitive(Setting.Name) and FEncryptSensitive then
            SettingsObj.AddPair(Setting.Name, EncryptValue(VarToStr(Setting.Value)))
          else
            SettingsObj.AddPair(Setting.Name, VarToStr(Setting.Value));
        end;
        stInteger: SettingsObj.AddPair(Setting.Name, TJSONNumber.Create(Setting.Value));
        stFloat: SettingsObj.AddPair(Setting.Name, TJSONNumber.Create(Setting.Value));
        stBoolean: SettingsObj.AddPair(Setting.Name, TJSONBool.Create(Setting.Value));
        stArray:
        begin
          var ArrayObj := TJSONArray.Create;
          var Values := VarArrayOf(Setting.Value);
          for var I := VarArrayLowBound(Values, 1) to VarArrayHighBound(Values, 1) do
            ArrayObj.AddElement(TJSONString.Create(VarToStr(Values[I])));
          SettingsObj.AddPair(Setting.Name, ArrayObj);
        end;
        stObject: 
        begin
          var ObjValue := TJSONObject.ParseJSONValue(VarToStr(Setting.Value));
          SettingsObj.AddPair(Setting.Name, ObjValue);
        end;
      end;
    end;
    JSONObj.AddPair('settings', SettingsObj);
    
    // Perfis
    ProfilesObj := TJSONObject.Create;
    for var Pair in FProfiles do
    begin
      Profile := Pair.Value;
      var ProfileObj := TJSONObject.Create;
      
      ProfileObj.AddPair('description', Profile.Description);
      ProfileObj.AddPair('created', DateToISO8601(Profile.CreatedDate));
      ProfileObj.AddPair('is_default', TJSONBool.Create(Profile.IsDefault));
      ProfileObj.AddPair('author', Profile.Author);
      
      var ProfileSettingsObj := TJSONObject.Create;
      for var SettingPair in Profile.Settings do
        ProfileSettingsObj.AddPair(SettingPair.Key, VarToStr(SettingPair.Value));
      ProfileObj.AddPair('settings', ProfileSettingsObj);
      
      ProfilesObj.AddPair(Profile.Name, ProfileObj);
    end;
    JSONObj.AddPair('profiles', ProfilesObj);
    
    JSONData := JSONObj.ToString;
    
  finally
    JSONObj.Free;
  end;
end;

procedure TSettingsManager.LoadFromINI(const INIFile: TIniFile);
var
  Sections: TStringList;
  Keys: TStringList;
  I, J: Integer;
  SettingName, Value: string;
  Setting: TSetting;
begin
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    INIFile.ReadSections(Sections);
    
    for I := 0 to Sections.Count - 1 do
    begin
      if Sections[I] = 'Settings' then
      begin
        Keys.Clear;
        INIFile.ReadSection(Sections[I], Keys);
        
        for J := 0 to Keys.Count - 1 do
        begin
          SettingName := Keys[J];
          Value := INIFile.ReadString(Sections[I], SettingName, '');
          
          if FSettings.TryGetValue(SettingName, Setting) then
          begin
            case Setting.SettingType of
              stString: Setting.Value := Value;
              stInteger: Setting.Value := StrToIntDef(Value, 0);
              stFloat: Setting.Value := StrToFloatDef(Value, 0.0);
              stBoolean: Setting.Value := StrToBoolDef(Value, False);
            end;
            
            FSettings.AddOrSetValue(SettingName, Setting);
          end;
        end;
      end;
    end;
    
  finally
    Keys.Free;
    Sections.Free;
  end;
end;

procedure TSettingsManager.SaveToINI(const INIFile: TIniFile);
var
  Setting: TSetting;
begin
  // Limpar seção de configurações
  INIFile.EraseSection('Settings');
  
  // Salvar configurações
  for var Pair in FSettings do
  begin
    Setting := Pair.Value;
    
    case Setting.SettingType of
      stString: 
      begin
        if IsSettingSensitive(Setting.Name) and FEncryptSensitive then
          INIFile.WriteString('Settings', Setting.Name, EncryptValue(VarToStr(Setting.Value)))
        else
          INIFile.WriteString('Settings', Setting.Name, VarToStr(Setting.Value));
      end;
      stInteger: INIFile.WriteInteger('Settings', Setting.Name, Setting.Value);
      stFloat: INIFile.WriteFloat('Settings', Setting.Name, Setting.Value);
      stBoolean: INIFile.WriteBool('Settings', Setting.Name, Setting.Value);
      stArray, stObject: INIFile.WriteString('Settings', Setting.Name, VarToStr(Setting.Value));
    end;
  end;
  
  // Metadados
  INIFile.WriteString('Metadata', 'Version', '1.0');
  INIFile.WriteString('Metadata', 'SavedAt', DateTimeToStr(Now));
  INIFile.WriteString('Metadata', 'CurrentProfile', FCurrentProfile);
end;

function TSettingsManager.GetSetting(const SettingName: string): Variant;
var
  Setting: TSetting;
begin
  // Verificar cache primeiro
  if FCacheEnabled and FSettingsCache.TryGetValue(SettingName, Result) then
    Exit;
  
  if FSettings.TryGetValue(SettingName, Setting) then
  begin
    Result := Setting.Value;
    
    // Atualizar cache
    if FCacheEnabled then
      UpdateCache(SettingName, Result);
  end
  else
    Result := Null;
end;

function TSettingsManager.GetSetting(const SettingName: string; const DefaultValue: Variant): Variant;
begin
  Result := GetSetting(SettingName);
  if VarIsNull(Result) then
    Result := DefaultValue;
end;

function TSettingsManager.SetSetting(const SettingName: string; const Value: Variant): Boolean;
var
  Setting: TSetting;
  OldValue: Variant;
  ValidationResult: TSettingsValidationResult;
begin
  Result := False;
  
  if not FSettings.TryGetValue(SettingName, Setting) then
    Exit;
  
  OldValue := Setting.Value;
  
  // Validar se habilitado
  if FValidationEnabled then
  begin
    ValidationResult := ValidateSetting(Setting, Value);
    if not ValidationResult.IsValid then
      Exit;
  end;
  
  // Converter para o tipo correto
  try
    Setting.Value := ConvertVariantToType(Value, Setting.SettingType);
    Setting.LastModified := Now;
    
    FSettings.AddOrSetValue(SettingName, Setting);
    
    // Marcar como modificado
    if FDirtySettings.IndexOf(SettingName) < 0 then
      FDirtySettings.Add(SettingName);
    
    // Atualizar cache
    if FCacheEnabled then
      UpdateCache(SettingName, Setting.Value);
    
    // Notificar mudança
    NotifySettingChanged(SettingName, OldValue, Setting.Value);
    
    // Salvar automaticamente se habilitado
    if FAutoSave then
      SaveSettings;
    
    Result := True;
    
  except
    on E: Exception do
    begin
      // Log error
    end;
  end;
end;

function TSettingsManager.ValidateSetting(const Setting: TSetting; const Value: Variant): TSettingsValidationResult;
var
  IsValid: Boolean;
  ErrorMsg: string;
begin
  Result.IsValid := True;
  SetLength(Result.Errors, 0);
  SetLength(Result.Warnings, 0);
  
  // Validação de tipo
  try
    ConvertVariantToType(Value, Setting.SettingType);
  except
    Result.IsValid := False;
    SetLength(Result.Errors, Length(Result.Errors) + 1);
    Result.Errors[High(Result.Errors)] := 'Tipo de valor inválido para ' + Setting.Name;
  end;
  
  // Validação obrigatória
  if Setting.Validation.Required and VarIsNull(Value) then
  begin
    Result.IsValid := False;
    SetLength(Result.Errors, Length(Result.Errors) + 1);
    Result.Errors[High(Result.Errors)] := Setting.Name + ' é obrigatório';
  end;
  
  // Validação de intervalo
  if not VarIsNull(Setting.Validation.MinValue) and (Value < Setting.Validation.MinValue) then
  begin
    Result.IsValid := False;
    SetLength(Result.Errors, Length(Result.Errors) + 1);
    Result.Errors[High(Result.Errors)] := Setting.Name + ' deve ser maior que ' + VarToStr(Setting.Validation.MinValue);
  end;
  
  if not VarIsNull(Setting.Validation.MaxValue) and (Value > Setting.Validation.MaxValue) then
  begin
    Result.IsValid := False;
    SetLength(Result.Errors, Length(Result.Errors) + 1);
    Result.Errors[High(Result.Errors)] := Setting.Name + ' deve ser menor que ' + VarToStr(Setting.Validation.MaxValue);
  end;
  
  // Validação de padrão
  if (Setting.Validation.Pattern <> '') and (Setting.SettingType = stString) then
  begin
    if not TRegEx.IsMatch(VarToStr(Value), Setting.Validation.Pattern) then
    begin
      Result.IsValid := False;
      SetLength(Result.Errors, Length(Result.Errors) + 1);
      Result.Errors[High(Result.Errors)] := Setting.Name + ' não atende ao padrão exigido';
    end;
  end;
  
  // Validação de valores permitidos
  if Length(Setting.Validation.AllowedValues) > 0 then
  begin
    var Found := False;
    for var AllowedValue in Setting.Validation.AllowedValues do
    begin
      if Value = AllowedValue then
      begin
        Found := True;
        Break;
      end;
    end;
    
    if not Found then
    begin
      Result.IsValid := False;
      SetLength(Result.Errors, Length(Result.Errors) + 1);
      Result.Errors[High(Result.Errors)] := Setting.Name + ' deve ser um dos valores permitidos';
    end;
  end;
  
  // Validação customizada
  if Assigned(Setting.Validation.CustomValidator) then
  begin
    if not Setting.Validation.CustomValidator(Value) then
    begin
      Result.IsValid := False;
      SetLength(Result.Errors, Length(Result.Errors) + 1);
      Result.Errors[High(Result.Errors)] := Setting.Name + ' falhou na validação customizada';
    end;
  end;
  
  // Evento de validação
  if Assigned(FOnValidation) then
  begin
    IsValid := True;
    ErrorMsg := '';
    FOnValidation(Self, Setting.Name, Value, IsValid, ErrorMsg);
    
    if not IsValid then
    begin
      Result.IsValid := False;
      SetLength(Result.Errors, Length(Result.Errors) + 1);
      Result.Errors[High(Result.Errors)] := ErrorMsg;
    end;
  end;
end;

procedure TSettingsManager.InitializeDefaultSettings;
begin
  // Inicializar configurações específicas da aplicação
  InitializeOllamaSettings;
  InitializeTrainingSettings;
  InitializeUISettings;
  InitializeAdvancedSettings;
  InitializeSecuritySettings;
end;

procedure TSettingsManager.InitializeOllamaSettings;
var
  Setting: TSetting;
begin
  // URL do Ollama
  Setting.Name := 'ollama.url';
  Setting.Value := 'http://localhost:11434';
  Setting.DefaultValue := 'http://localhost:11434';
  Setting.SettingType := stString;
  Setting.Description := 'URL do servidor Ollama';
  Setting.Category := 'Ollama';
  Setting.Validation.Required := True;
  Setting.Validation.Pattern := '^https?://.+';
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := True;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // API Key
  Setting.Name := 'ollama.api_key';
  Setting.Value := '';
  Setting.DefaultValue := '';
  Setting.SettingType := stString;
  Setting.Description := 'Chave de API do Ollama (se necessária)';
  Setting.Category := 'Ollama';
  Setting.Validation.Required := False;
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Timeout
  Setting.Name := 'ollama.timeout';
  Setting.Value := 300;
  Setting.DefaultValue := 300;
  Setting.SettingType := stInteger;
  Setting.Description := 'Timeout em segundos para requisições';
  Setting.Category := 'Ollama';
  Setting.Validation.MinValue := 10;
  Setting.Validation.MaxValue := 3600;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Retry Count
  Setting.Name := 'ollama.retry_count';
  Setting.Value := 3;
  Setting.DefaultValue := 3;
  Setting.SettingType := stInteger;
  Setting.Description := 'Número de tentativas em caso de falha';
  Setting.Category := 'Ollama';
  Setting.Validation.MinValue := 1;
  Setting.Validation.MaxValue := 10;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
end;

procedure TSettingsManager.InitializeTrainingSettings;
var
  Setting: TSetting;
begin
  // Epochs padrão
  Setting.Name := 'training.default_epochs';
  Setting.Value := 10;
  Setting.DefaultValue := 10;
  Setting.SettingType := stInteger;
  Setting.Description := 'Número padrão de épocas para treinamento';
  Setting.Category := 'Treinamento';
  Setting.Validation.MinValue := 1;
  Setting.Validation.MaxValue := 1000;
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Batch Size padrão
  Setting.Name := 'training.default_batch_size';
  Setting.Value := 32;
  Setting.DefaultValue := 32;
  Setting.SettingType := stInteger;
  Setting.Description := 'Tamanho padrão do batch para treinamento';
  Setting.Category := 'Treinamento';
  Setting.Validation.MinValue := 1;
  Setting.Validation.MaxValue := 1024;
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Learning Rate padrão
  Setting.Name := 'training.default_learning_rate';
  Setting.Value := 0.001;
  Setting.DefaultValue := 0.001;
  Setting.SettingType := stFloat;
  Setting.Description := 'Taxa de aprendizado padrão';
  Setting.Category := 'Treinamento';
  Setting.Validation.MinValue := 0.0001;
  Setting.Validation.MaxValue := 1.0;
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Auto Save Checkpoints
  Setting.Name := 'training.auto_save_checkpoints';
  Setting.Value := True;
  Setting.DefaultValue := True;
  Setting.SettingType := stBoolean;
  Setting.Description := 'Salvar checkpoints automaticamente durante o treinamento';
  Setting.Category := 'Treinamento';
  Setting.Validation.Required := False;
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Checkpoint Interval
  Setting.Name := 'training.checkpoint_interval';
  Setting.Value := 5;
  Setting.DefaultValue := 5;
  Setting.SettingType := stInteger;
  Setting.Description := 'Intervalo (em épocas) para salvar checkpoints';
  Setting.Category := 'Treinamento';
  Setting.Validation.MinValue := 1;
  Setting.Validation.MaxValue := 100;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
end;

procedure TSettingsManager.InitializeUISettings;
var
  Setting: TSetting;
begin
  // Tema
  Setting.Name := 'ui.theme';
  Setting.Value := 'default';
  Setting.DefaultValue := 'default';
  Setting.SettingType := stString;
  Setting.Description := 'Tema da interface';
  Setting.Category := 'Interface';
  Setting.Validation.AllowedValues := VarArrayOf(['default', 'dark', 'light', 'blue', 'green']);
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := True;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Idioma
  Setting.Name := 'ui.language';
  Setting.Value := 'pt-BR';
  Setting.DefaultValue := 'pt-BR';
  Setting.SettingType := stString;
  Setting.Description := 'Idioma da interface';
  Setting.Category := 'Interface';
  Setting.Validation.AllowedValues := VarArrayOf(['pt-BR', 'en-US', 'es-ES', 'fr-FR']);
  Setting.IsAdvanced := False;
  Setting.RequiresRestart := True;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Auto Save Interval
  Setting.Name := 'ui.auto_save_interval';
  Setting.Value := 300;
  Setting.DefaultValue := 300;
  Setting.SettingType := stInteger;
  Setting.Description := 'Intervalo de salvamento automático (segundos)';
  Setting.Category := 'Interface';
  Setting.Validation.MinValue := 30;
  Setting.Validation.MaxValue := 3600;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
end;

procedure TSettingsManager.InitializeAdvancedSettings;
var
  Setting: TSetting;
begin
  // Debug Mode
  Setting.Name := 'advanced.debug_mode';
  Setting.Value := False;
  Setting.DefaultValue := False;
  Setting.SettingType := stBoolean;
  Setting.Description := 'Habilitar modo de debug';
  Setting.Category := 'Avançado';
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := True;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Log Level
  Setting.Name := 'advanced.log_level';
  Setting.Value := 'info';
  Setting.DefaultValue := 'info';
  Setting.SettingType := stString;
  Setting.Description := 'Nível de log';
  Setting.Category := 'Avançado';
  Setting.Validation.AllowedValues := VarArrayOf(['debug', 'info', 'warning', 'error']);
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Max Memory Usage
  Setting.Name := 'advanced.max_memory_mb';
  Setting.Value := 8192;
  Setting.DefaultValue := 8192;
  Setting.SettingType := stInteger;
  Setting.Description := 'Uso máximo de memória em MB';
  Setting.Category := 'Avançado';
  Setting.Validation.MinValue := 1024;
  Setting.Validation.MaxValue := 65536;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := True;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
end;

procedure TSettingsManager.InitializeSecuritySettings;
var
  Setting: TSetting;
begin
  // Encrypt Sensitive Data
  Setting.Name := 'security.encrypt_sensitive';
  Setting.Value := True;
  Setting.DefaultValue := True;
  Setting.SettingType := stBoolean;
  Setting.Description := 'Criptografar dados sensíveis';
  Setting.Category := 'Segurança';
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
  
  // Session Timeout
  Setting.Name := 'security.session_timeout';
  Setting.Value := 3600;
  Setting.DefaultValue := 3600;
  Setting.SettingType := stInteger;
  Setting.Description := 'Timeout da sessão em segundos';
  Setting.Category := 'Segurança';
  Setting.Validation.MinValue := 300;
  Setting.Validation.MaxValue := 86400;
  Setting.IsAdvanced := True;
  Setting.RequiresRestart := False;
  Setting.LastModified := Now;
  RegisterSetting(Setting);
end;

procedure TSettingsManager.InitializeCategories;
var
  Category: TSettingsCategory;
begin
  // Categoria Ollama
  Category.Name := 'Ollama';
  Category.DisplayName := 'Configurações do Ollama';
  Category.Description := 'Configurações de conexão e API do Ollama';
  Category.Icon := 'ollama';
  Category.Order := 1;
  AddCategory(Category);
  
  // Categoria Treinamento
  Category.Name := 'Treinamento';
  Category.DisplayName := 'Configurações de Treinamento';
  Category.Description := 'Parâmetros padrão para treinamento de modelos';
  Category.Icon := 'training';
  Category.Order := 2;
  AddCategory(Category);
  
  // Categoria Interface
  Category.Name := 'Interface';
  Category.DisplayName := 'Interface do Usuário';
  Category.Description := 'Configurações de aparência e comportamento da UI';
  Category.Icon := 'ui';
  Category.Order := 3;
  AddCategory(Category);
  
  // Categoria Avançado
  Category.Name := 'Avançado';
  Category.DisplayName := 'Configurações Avançadas';
  Category.Description := 'Configurações técnicas e de desenvolvimento';
  Category.Icon := 'advanced';
  Category.Order := 4;
  AddCategory(Category);
  
  // Categoria Segurança
  Category.Name := 'Segurança';
  Category.DisplayName := 'Configurações de Segurança';
  Category.Description := 'Configurações de segurança e privacidade';
  Category.Icon := 'security';
  Category.Order := 5;
  AddCategory(Category);
end;

// Implementações básicas para métodos restantes

function TSettingsManager.RegisterSetting(const Setting: TSetting): Boolean;
begin
  Result := True;
  FSettings.AddOrSetValue(Setting.Name, Setting);
end;

function TSettingsManager.AddCategory(const Category: TSettingsCategory): Boolean;
begin
  Result := True;
  FCategories.Add(Category);
end;

function TSettingsManager.EncryptValue(const Value: string): string;
begin
  // Implementação básica de criptografia
  Result := TNetEncoding.Base64.Encode(Value);
end;

function TSettingsManager.DecryptValue(const Value: string): string;
begin
  // Implementação básica de descriptografia
  try
    Result := TNetEncoding.Base64.Decode(Value);
  except
    Result := Value;
  end;
end;

function TSettingsManager.IsSettingSensitive(const SettingName: string): Boolean;
begin
  Result := ContainsText(SettingName, 'password') or 
            ContainsText(SettingName, 'key') or
            ContainsText(SettingName, 'token') or
            ContainsText(SettingName, 'secret');
end;

procedure TSettingsManager.CreateBackup;
var
  BackupFile: string;
  SourceFile: string;
begin
  try
    SourceFile := GetConfigFileName;
    BackupFile := TPath.Combine(GetBackupPath, 
      Format('settings_backup_%s.%s', [FormatDateTime('yyyymmdd_hhnnss', Now), FConfigFormat]));
    
    ForceDirectories(GetBackupPath);
    TFile.Copy(SourceFile, BackupFile, True);
    
    CleanupBackups;
  except
    // Log error
  end;
end;

function TSettingsManager.GetBackupPath: string;
begin
  Result := TPath.Combine(FConfigPath, 'backups');
end;

procedure TSettingsManager.CleanupBackups;
var
  BackupFiles: TStringDynArray;
  I: Integer;
begin
  try
    BackupFiles := TDirectory.GetFiles(GetBackupPath, 'settings_backup_*.*');
    
    if Length(BackupFiles) > FMaxBackups then
    begin
      // Ordenar por data de criação e deletar os mais antigos
      TArray.Sort<string>(BackupFiles);
      
      for I := 0 to Length(BackupFiles) - FMaxBackups - 1 do
        TFile.Delete(BackupFiles[I]);
    end;
  except
    // Log error
  end;
end;

function TSettingsManager.ConvertVariantToType(const Value: Variant; TargetType: TSettingType): Variant;
begin
  case TargetType of
    stString: Result := VarToStr(Value);
    stInteger: Result := VarAsType(Value, varInteger);
    stFloat: Result := VarAsType(Value, varDouble);
    stBoolean: Result := VarAsType(Value, varBoolean);
    stArray: Result := Value; // Assume já está no formato correto
    stObject: Result := Value; // Assume já está no formato correto
  else
    Result := Value;
  end;
end;

procedure TSettingsManager.UpdateCache(const SettingName: string; const Value: Variant);
begin
  if FCacheEnabled then
    FSettingsCache.AddOrSetValue(SettingName, Value);
end;

procedure TSettingsManager.InvalidateCache;
begin
  FSettingsCache.Clear;
end;

procedure TSettingsManager.NotifySettingChanged(const SettingName: string; const OldValue, NewValue: Variant);
begin
  if Assigned(FOnSettingChanged) then
    FOnSettingChanged(Self, SettingName, OldValue, NewValue);
end;

function TSettingsManager.HasUnsavedChanges: Boolean;
begin
  Result := FDirtySettings.Count > 0;
end;

// Implementações básicas para métodos restantes (placeholders)
function TSettingsManager.ReloadSettings: Boolean; begin Result := LoadSettings; end;
function TSettingsManager.ResetToDefaults: Boolean; begin Result := True; end;
function TSettingsManager.ResetSetting(const SettingName: string): Boolean; begin Result := True; end;
function TSettingsManager.GetSettingAsString(const SettingName: string; const Default: string): string; begin Result := VarToStrDef(GetSetting(SettingName), Default); end;
function TSettingsManager.GetSettingAsInteger(const SettingName: string; const Default: Integer): Integer; begin Result := VarToIntDef(GetSetting(SettingName), Default); end;
function TSettingsManager.GetSettingAsFloat(const SettingName: string; const Default: Double): Double; begin Result := VarToFloatDef(GetSetting(SettingName), Default); end;
function TSettingsManager.GetSettingAsBoolean(const SettingName: string; const Default: Boolean): Boolean; begin Result := VarToBoolDef(GetSetting(SettingName), Default); end;
function TSettingsManager.GetSettingAsArray(const SettingName: string): TArray<Variant>; begin SetLength(Result, 0); end;
function TSettingsManager.GetSettingAsObject(const SettingName: string): TJSONObject; begin Result := TJSONObject.Create; end;

function TSettingsManager.SetSettingString(const SettingName, Value: string): Boolean; begin Result := SetSetting(SettingName, Value); end;
function TSettingsManager.SetSettingInteger(const SettingName: string; const Value: Integer): Boolean; begin Result := SetSetting(SettingName, Value); end;
function TSettingsManager.SetSettingFloat(const SettingName: string; const Value: Double): Boolean; begin Result := SetSetting(SettingName, Value); end;
function TSettingsManager.SetSettingBoolean(const SettingName: string; const Value: Boolean): Boolean; begin Result := SetSetting(SettingName, Value); end;
function TSettingsManager.SetSettingArray(const SettingName: string; const Value: TArray<Variant>): Boolean; begin Result := SetSetting(SettingName, Value); end;
function TSettingsManager.SetSettingObject(const SettingName: string; const Value: TJSONObject): Boolean; begin Result := SetSetting(SettingName, Value.ToString); end;

// Continuar com implementações básicas...
function TSettingsManager.SettingExists(const SettingName: string): Boolean; begin Result := FSettings.ContainsKey(SettingName); end;
function TSettingsManager.UnregisterSetting(const SettingName: string): Boolean; begin Result := FSettings.Remove(SettingName); end;
function TSettingsManager.GetSettingInfo(const SettingName: string): TSetting; begin FSettings.TryGetValue(SettingName, Result); end;
function TSettingsManager.GetAllSettings: TArray<TSetting>; begin SetLength(Result, 0); end;
function TSettingsManager.GetSettingsByCategory(const Category: string): TArray<TSetting>; begin SetLength(Result, 0); end;
function TSettingsManager.GetAdvancedSettings: TArray<TSetting>; begin SetLength(Result, 0); end;
function TSettingsManager.GetRequiredSettings: TArray<TSetting>; begin SetLength(Result, 0); end;

end.