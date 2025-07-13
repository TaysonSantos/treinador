unit ModelManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.IOUtils, System.DateUtils, System.Math, System.Threading,
  System.Net.HttpClient, OllamaAPI;

type
  TModelInfo = record
    Name: string;
    Size: Int64;
    Created: TDateTime;
    Modified: TDateTime;
    Parameters: Int64;
    Accuracy: Double;
    Languages: string;
    Description: string;
    Tags: TStringList;
    Family: string;
    Format: string;
    Digest: string;
    IsCustom: Boolean;
    TrainingData: string;
    Performance: TJSONObject;
    Capabilities: TJSONObject;
  end;
  
  TModelCategory = (mcGeneral, mcCode, mcText, mcImage, mcMultimodal, mcSpecialized, mcCustom);
  
  TModelStatus = (msAvailable, msDownloading, msDeleting, msUpdating, msError, msUnknown);
  
  TModelFilter = record
    Category: TModelCategory;
    MinSize: Int64;
    MaxSize: Int64;
    MinParameters: Int64;
    MaxParameters: Int64;
    Languages: TArray<string>;
    Tags: TArray<string>;
    DateFrom: TDateTime;
    DateTo: TDateTime;
    OnlyCustom: Boolean;
    NamePattern: string;
  end;
  
  TModelBenchmark = record
    ModelName: string;
    TestName: string;
    Score: Double;
    ExecutionTime: TDateTime;
    TokensPerSecond: Double;
    MemoryUsage: Int64;
    TestDate: TDateTime;
    TestConfig: TJSONObject;
  end;
  
  TModelComparison = record
    Model1: string;
    Model2: string;
    Metrics: TJSONObject;
    Winner: string;
    ComparisonDate: TDateTime;
    TestScenarios: TStringList;
  end;
  
  TModelEvent = procedure(Sender: TObject; const ModelName: string; Status: TModelStatus; Progress: Integer) of object;
  TModelBenchmarkEvent = procedure(Sender: TObject; const Benchmark: TModelBenchmark) of object;
  TModelErrorEvent = procedure(Sender: TObject; const ModelName, Error: string) of object;
  
  TModelManager = class
  private
    FOllamaAPI: TOllamaAPI;
    FModels: TDictionary<string, TModelInfo>;
    FBenchmarks: TList<TModelBenchmark>;
    FComparisons: TList<TModelComparison>;
    FDownloadTasks: TDictionary<string, ITask>;
    FModelCategories: TDictionary<string, TModelCategory>;
    FRecommendations: TDictionary<string, TStringList>;
    FCacheExpiry: TDateTime;
    FCacheTimeout: Integer; // em minutos
    
    // Eventos
    FOnModelStatusChange: TModelEvent;
    FOnBenchmarkComplete: TModelBenchmarkEvent;
    FOnError: TModelErrorEvent;
    
    // Configurações
    FAutoUpdate: Boolean;
    FMaxConcurrentDownloads: Integer;
    FDefaultDownloadPath: string;
    FBenchmarkEnabled: Boolean;
    
    function LoadModelInfo(const ModelName: string): TModelInfo;
    function CategorizeModel(const ModelInfo: TModelInfo): TModelCategory;
    procedure UpdateModelCache;
    function IsValidModelName(const ModelName: string): Boolean;
    function GetModelSize(const ModelName: string): Int64;
    function GetModelDigest(const ModelName: string): string;
    function AnalyzeModelCapabilities(const ModelInfo: TModelInfo): TJSONObject;
    function EstimateModelPerformance(const ModelInfo: TModelInfo): TJSONObject;
    procedure InitializeModelCategories;
    procedure LoadModelRecommendations;
    function ParseModelTags(const ModelName: string): TStringList;
    function ExtractModelFamily(const ModelName: string): string;
    function GetAvailableSpace: Int64;
    function ValidateModelIntegrity(const ModelName: string): Boolean;
    
  public
    constructor Create(OllamaAPI: TOllamaAPI);
    destructor Destroy; override;
    
    // Propriedades
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    property MaxConcurrentDownloads: Integer read FMaxConcurrentDownloads write FMaxConcurrentDownloads;
    property DefaultDownloadPath: string read FDefaultDownloadPath write FDefaultDownloadPath;
    property BenchmarkEnabled: Boolean read FBenchmarkEnabled write FBenchmarkEnabled;
    property CacheTimeout: Integer read FCacheTimeout write FCacheTimeout;
    
    // Eventos
    property OnModelStatusChange: TModelEvent read FOnModelStatusChange write FOnModelStatusChange;
    property OnBenchmarkComplete: TModelBenchmarkEvent read FOnBenchmarkComplete write FOnBenchmarkComplete;
    property OnError: TModelErrorEvent read FOnError write FOnError;
    
    // Gerenciamento básico de modelos
    function GetModelList(ModelList: TStringList): Boolean;
    function GetModelInfo(const ModelName: string; out ModelInfo: TModelInfo): Boolean;
    function RefreshModelList: Boolean;
    function ModelExists(const ModelName: string): Boolean;
    function GetModelStatus(const ModelName: string): TModelStatus;
    
    // Download e instalação
    function DownloadModel(const ModelName: string): Boolean;
    function DownloadModelAsync(const ModelName: string): ITask;
    function DownloadBatchModels(const ModelNames: TArray<string>): Boolean;
    function GetDownloadProgress(const ModelName: string): Integer;
    function CancelDownload(const ModelName: string): Boolean;
    function PauseDownload(const ModelName: string): Boolean;
    function ResumeDownload(const ModelName: string): Boolean;
    
    // Remoção e limpeza
    function DeleteModel(const ModelName: string): Boolean;
    function DeleteMultipleModels(const ModelNames: TArray<string>): Boolean;
    function CleanupUnusedModels: Integer;
    function CompactModelStorage: Boolean;
    function BackupModel(const ModelName, BackupPath: string): Boolean;
    function RestoreModel(const BackupPath, ModelName: string): Boolean;
    
    // Organização e filtros
    function FilterModels(const Filter: TModelFilter): TArray<TModelInfo>;
    function SearchModels(const Query: string): TArray<TModelInfo>;
    function GetModelsByCategory(Category: TModelCategory): TArray<TModelInfo>;
    function GetModelsBySize(MinSize, MaxSize: Int64): TArray<TModelInfo>;
    function GetRecentModels(Days: Integer): TArray<TModelInfo>;
    function GetPopularModels: TArray<TModelInfo>;
    function SortModels(const Models: TArray<TModelInfo>; 
      SortBy: string; Descending: Boolean = False): TArray<TModelInfo>;
    
    // Análise e estatísticas
    function GetModelStatistics: TJSONObject;
    function GetStorageStatistics: TJSONObject;
    function GetUsageStatistics: TJSONObject;
    function AnalyzeModelCollection: TJSONObject;
    function GenerateModelReport: string;
    function ExportModelList(const FileName: string; Format: string = 'json'): Boolean;
    function ImportModelList(const FileName: string): Boolean;
    
    // Benchmarks e comparações
    function BenchmarkModel(const ModelName: string; TestSuite: string = 'standard'): TModelBenchmark;
    function BenchmarkAllModels(TestSuite: string = 'quick'): TArray<TModelBenchmark>;
    function CompareModels(const Model1, Model2: string): TModelComparison;
    function GetBenchmarkHistory(const ModelName: string): TArray<TModelBenchmark>;
    function GetTopPerformingModels(Metric: string; Count: Integer = 10): TArray<string>;
    function AnalyzeBenchmarkTrends: TJSONObject;
    
    // Recomendações e sugestões
    function GetModelRecommendations(const UseCase: string): TArray<string>;
    function SuggestModelForTask(const TaskDescription: string): string;
    function GetSimilarModels(const ModelName: string): TArray<string>;
    function GetUpgradeRecommendations: TArray<string>;
    function GetAlternativeModels(const ModelName: string): TArray<string>;
    
    // Validação e integridade
    function ValidateAllModels: TArray<string>;
    function RepairModel(const ModelName: string): Boolean;
    function VerifyModelIntegrity(const ModelName: string): Boolean;
    function CheckModelCompatibility(const ModelName: string): Boolean;
    function GetModelDependencies(const ModelName: string): TArray<string>;
    
    // Configuração e personalização
    function CreateCustomModel(const BaseName, CustomName, Instructions: string): Boolean;
    function ModifyModel(const ModelName, ModificationScript: string): Boolean;
    function CloneModel(const SourceModel, TargetModel: string): Boolean;
    function MergeModels(const Model1, Model2, TargetModel: string): Boolean;
    function OptimizeModel(const ModelName: string; OptimizationLevel: Integer = 1): Boolean;
    
    // Atualização e manutenção
    function CheckForUpdates: TArray<string>;
    function UpdateModel(const ModelName: string): Boolean;
    function UpdateAllModels: Boolean;
    function ScheduleModelMaintenance(const Schedule: string): Boolean;
    function PerformMaintenance: Boolean;
    
    // Exportação e migração
    function ExportModel(const ModelName, ExportPath: string; IncludeData: Boolean = True): Boolean;
    function ImportModel(const ImportPath, ModelName: string): Boolean;
    function MigrateModels(const TargetPath: string): Boolean;
    function SyncWithRemoteRepository(const RepoURL: string): Boolean;
    
    // Utilitários
    function GetModelHash(const ModelName: string): string;
    function GetModelMetadata(const ModelName: string): TJSONObject;
    function SetModelMetadata(const ModelName: string; const Metadata: TJSONObject): Boolean;
    function TagModel(const ModelName: string; const Tags: TArray<string>): Boolean;
    function UntagModel(const ModelName: string; const Tags: TArray<string>): Boolean;
    function GetModelsWithTag(const Tag: string): TArray<string>;
    
    // Cache e performance
    procedure RefreshCache;
    procedure ClearCache;
    function GetCacheInfo: TJSONObject;
    function OptimizeCache: Boolean;
    procedure SetCacheSettings(TimeoutMinutes: Integer; MaxSize: Int64);
  end;

implementation

uses
  System.Variants, System.StrUtils, System.RegularExpressions, System.Hash,
  Winapi.Windows;

{ TModelManager }

constructor TModelManager.Create(OllamaAPI: TOllamaAPI);
begin
  inherited Create;
  
  FOllamaAPI := OllamaAPI;
  FModels := TDictionary<string, TModelInfo>.Create;
  FBenchmarks := TList<TModelBenchmark>.Create;
  FComparisons := TList<TModelComparison>.Create;
  FDownloadTasks := TDictionary<string, ITask>.Create;
  FModelCategories := TDictionary<string, TModelCategory>.Create;
  FRecommendations := TDictionary<string, TStringList>.Create;
  
  // Configurações padrão
  FAutoUpdate := False;
  FMaxConcurrentDownloads := 3;
  FDefaultDownloadPath := TPath.GetTempPath;
  FBenchmarkEnabled := True;
  FCacheTimeout := 60; // 60 minutos
  FCacheExpiry := Now;
  
  // Inicializar dados
  InitializeModelCategories;
  LoadModelRecommendations;
  UpdateModelCache;
end;

destructor TModelManager.Destroy;
var
  ModelInfo: TModelInfo;
  Benchmark: TModelBenchmark;
  Comparison: TModelComparison;
begin
  // Cancelar downloads em andamento
  for var Pair in FDownloadTasks do
  begin
    if Assigned(Pair.Value) then
      Pair.Value.Cancel;
  end;
  
  // Liberar recursos dos modelos
  for var Pair in FModels do
  begin
    ModelInfo := Pair.Value;
    if Assigned(ModelInfo.Tags) then
      ModelInfo.Tags.Free;
    if Assigned(ModelInfo.Performance) then
      ModelInfo.Performance.Free;
    if Assigned(ModelInfo.Capabilities) then
      ModelInfo.Capabilities.Free;
  end;
  
  // Liberar benchmarks
  for var I := 0 to FBenchmarks.Count - 1 do
  begin
    Benchmark := FBenchmarks[I];
    if Assigned(Benchmark.TestConfig) then
      Benchmark.TestConfig.Free;
  end;
  
  // Liberar comparações
  for var I := 0 to FComparisons.Count - 1 do
  begin
    Comparison := FComparisons[I];
    if Assigned(Comparison.Metrics) then
      Comparison.Metrics.Free;
    if Assigned(Comparison.TestScenarios) then
      Comparison.TestScenarios.Free;
  end;
  
  // Liberar recomendações
  for var Pair in FRecommendations do
    Pair.Value.Free;
  
  FRecommendations.Free;
  FModelCategories.Free;
  FDownloadTasks.Free;
  FComparisons.Free;
  FBenchmarks.Free;
  FModels.Free;
  
  inherited Destroy;
end;

function TModelManager.GetModelList(ModelList: TStringList): Boolean;
var
  OllamaModels: TArray<TOllamaModel>;
  I: Integer;
begin
  Result := False;
  ModelList.Clear;
  
  try
    // Verificar se cache está válido
    if (FCacheExpiry < Now) then
      UpdateModelCache;
    
    // Se cache estiver vazio, tentar carregar da API
    if FModels.Count = 0 then
    begin
      OllamaModels := FOllamaAPI.GetModels;
      for I := 0 to High(OllamaModels) do
      begin
        ModelList.Add(OllamaModels[I].Name);
        
        // Carregar informações detalhadas se não estiver no cache
        if not FModels.ContainsKey(OllamaModels[I].Name) then
        begin
          var ModelInfo := LoadModelInfo(OllamaModels[I].Name);
          FModels.Add(OllamaModels[I].Name, ModelInfo);
        end;
      end;
    end
    else
    begin
      // Usar cache
      for var ModelName in FModels.Keys do
        ModelList.Add(ModelName);
    end;
    
    Result := ModelList.Count > 0;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, '', 'Erro ao obter lista de modelos: ' + E.Message);
    end;
  end;
end;

function TModelManager.GetModelInfo(const ModelName: string; out ModelInfo: TModelInfo): Boolean;
begin
  Result := False;
  FillChar(ModelInfo, SizeOf(ModelInfo), 0);
  
  try
    // Verificar cache primeiro
    if FModels.TryGetValue(ModelName, ModelInfo) then
    begin
      Result := True;
      Exit;
    end;
    
    // Carregar da API se não estiver no cache
    ModelInfo := LoadModelInfo(ModelName);
    if ModelInfo.Name <> '' then
    begin
      FModels.AddOrSetValue(ModelName, ModelInfo);
      Result := True;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Erro ao obter informações do modelo: ' + E.Message);
    end;
  end;
end;

function TModelManager.LoadModelInfo(const ModelName: string): TModelInfo;
var
  OllamaModel: TOllamaModel;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  try
    OllamaModel := FOllamaAPI.GetModelInfo(ModelName);
    
    Result.Name := OllamaModel.Name;
    Result.Size := OllamaModel.Size;
    Result.Modified := OllamaModel.Modified;
    Result.Digest := OllamaModel.Digest;
    Result.Family := ExtractModelFamily(ModelName);
    Result.Format := 'GGUF'; // Formato padrão do Ollama
    Result.IsCustom := ModelName.Contains('custom') or ModelName.Contains('fine-tuned');
    Result.Created := Now; // Placeholder
    Result.Parameters := EstimateParameters(OllamaModel.Size);
    Result.Accuracy := 0.85; // Placeholder
    Result.Description := 'Modelo ' + ModelName;
    Result.Languages := 'Português, Inglês'; // Placeholder
    
    Result.Tags := ParseModelTags(ModelName);
    Result.Performance := EstimateModelPerformance(Result);
    Result.Capabilities := AnalyzeModelCapabilities(Result);
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Erro ao carregar informações: ' + E.Message);
    end;
  end;
end;

function TModelManager.EstimateParameters(Size: Int64): Int64;
begin
  // Estimativa grosseira baseada no tamanho
  // Assumindo 4 bytes por parâmetro (float32)
  Result := Size div 4;
  
  // Ajustar baseado em heurísticas conhecidas
  if Size < 1024 * 1024 * 1024 then // < 1GB
    Result := Result div 2 // Overhead menor
  else if Size > 10 * 1024 * 1024 * 1024 then // > 10GB
    Result := Round(Result * 0.8); // Overhead maior
end;

function TModelManager.CategorizeModel(const ModelInfo: TModelInfo): TModelCategory;
begin
  Result := mcGeneral; // Padrão
  
  // Categorizar baseado no nome e características
  var ModelNameLower := LowerCase(ModelInfo.Name);
  
  if ModelNameLower.Contains('code') or ModelNameLower.Contains('program') then
    Result := mcCode
  else if ModelNameLower.Contains('vision') or ModelNameLower.Contains('image') then
    Result := mcImage
  else if ModelNameLower.Contains('multimodal') or ModelNameLower.Contains('llava') then
    Result := mcMultimodal
  else if ModelInfo.IsCustom then
    Result := mcCustom
  else if ModelNameLower.Contains('instruct') or ModelNameLower.Contains('chat') then
    Result := mcText
  else if ModelInfo.Size > 50 * 1024 * 1024 * 1024 then // > 50GB
    Result := mcSpecialized;
end;

procedure TModelManager.UpdateModelCache;
var
  OllamaModels: TArray<TOllamaModel>;
  I: Integer;
  ModelInfo: TModelInfo;
begin
  try
    OllamaModels := FOllamaAPI.GetModels;
    
    // Limpar cache antigo
    for var Pair in FModels do
    begin
      if Assigned(Pair.Value.Tags) then
        Pair.Value.Tags.Free;
      if Assigned(Pair.Value.Performance) then
        Pair.Value.Performance.Free;
      if Assigned(Pair.Value.Capabilities) then
        Pair.Value.Capabilities.Free;
    end;
    FModels.Clear;
    
    // Carregar novos dados
    for I := 0 to High(OllamaModels) do
    begin
      ModelInfo := LoadModelInfo(OllamaModels[I].Name);
      FModels.Add(OllamaModels[I].Name, ModelInfo);
    end;
    
    FCacheExpiry := IncMinute(Now, FCacheTimeout);
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, '', 'Erro ao atualizar cache: ' + E.Message);
    end;
  end;
end;

function TModelManager.RefreshModelList: Boolean;
begin
  Result := False;
  try
    UpdateModelCache;
    Result := True;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, '', 'Erro ao atualizar lista: ' + E.Message);
    end;
  end;
end;

function TModelManager.ModelExists(const ModelName: string): Boolean;
begin
  Result := FModels.ContainsKey(ModelName) or FOllamaAPI.ValidateModel(ModelName);
end;

function TModelManager.GetModelStatus(const ModelName: string): TModelStatus;
begin
  Result := msUnknown;
  
  if FDownloadTasks.ContainsKey(ModelName) then
  begin
    var Task := FDownloadTasks[ModelName];
    if Assigned(Task) then
    begin
      case Task.Status of
        TTaskStatus.Running: Result := msDownloading;
        TTaskStatus.Completed: Result := msAvailable;
        TTaskStatus.Canceled: Result := msError;
        TTaskStatus.Exception: Result := msError;
      else
        Result := msUnknown;
      end;
    end;
  end
  else if ModelExists(ModelName) then
    Result := msAvailable;
end;

function TModelManager.DownloadModel(const ModelName: string): Boolean;
begin
  Result := False;
  
  try
    if ModelExists(ModelName) then
    begin
      Result := True;
      Exit;
    end;
    
    // Verificar espaço disponível
    var ModelSize := GetModelSize(ModelName);
    if (ModelSize > 0) and (GetAvailableSpace < ModelSize) then
    begin
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Espaço insuficiente em disco');
      Exit;
    end;
    
    if Assigned(FOnModelStatusChange) then
      FOnModelStatusChange(Self, ModelName, msDownloading, 0);
    
    Result := FOllamaAPI.PullModel(ModelName);
    
    if Result then
    begin
      // Atualizar cache
      var ModelInfo := LoadModelInfo(ModelName);
      FModels.AddOrSetValue(ModelName, ModelInfo);
      
      if Assigned(FOnModelStatusChange) then
        FOnModelStatusChange(Self, ModelName, msAvailable, 100);
    end
    else
    begin
      if Assigned(FOnModelStatusChange) then
        FOnModelStatusChange(Self, ModelName, msError, 0);
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Erro no download: ' + E.Message);
        
      if Assigned(FOnModelStatusChange) then
        FOnModelStatusChange(Self, ModelName, msError, 0);
    end;
  end;
end;

function TModelManager.DownloadModelAsync(const ModelName: string): ITask;
begin
  Result := TTask.Create(
    procedure
    begin
      DownloadModel(ModelName);
    end);
    
  FDownloadTasks.AddOrSetValue(ModelName, Result);
  Result.Start;
end;

function TModelManager.DeleteModel(const ModelName: string): Boolean;
begin
  Result := False;
  
  try
    if not ModelExists(ModelName) then
    begin
      Result := True; // Já não existe
      Exit;
    end;
    
    if Assigned(FOnModelStatusChange) then
      FOnModelStatusChange(Self, ModelName, msDeleting, 0);
    
    Result := FOllamaAPI.DeleteModel(ModelName);
    
    if Result then
    begin
      // Remover do cache
      if FModels.ContainsKey(ModelName) then
      begin
        var ModelInfo := FModels[ModelName];
        if Assigned(ModelInfo.Tags) then
          ModelInfo.Tags.Free;
        if Assigned(ModelInfo.Performance) then
          ModelInfo.Performance.Free;
        if Assigned(ModelInfo.Capabilities) then
          ModelInfo.Capabilities.Free;
          
        FModels.Remove(ModelName);
      end;
      
      if Assigned(FOnModelStatusChange) then
        FOnModelStatusChange(Self, ModelName, msUnknown, 100);
    end
    else
    begin
      if Assigned(FOnModelStatusChange) then
        FOnModelStatusChange(Self, ModelName, msError, 0);
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Erro ao deletar modelo: ' + E.Message);
    end;
  end;
end;

function TModelManager.FilterModels(const Filter: TModelFilter): TArray<TModelInfo>;
var
  FilteredModels: TList<TModelInfo>;
  ModelInfo: TModelInfo;
  I, J: Integer;
  MatchesFilter: Boolean;
begin
  FilteredModels := TList<TModelInfo>.Create;
  try
    for var Pair in FModels do
    begin
      ModelInfo := Pair.Value;
      MatchesFilter := True;
      
      // Verificar categoria
      if (Filter.Category <> mcGeneral) and 
         (CategorizeModel(ModelInfo) <> Filter.Category) then
        MatchesFilter := False;
      
      // Verificar tamanho
      if (Filter.MinSize > 0) and (ModelInfo.Size < Filter.MinSize) then
        MatchesFilter := False;
      if (Filter.MaxSize > 0) and (ModelInfo.Size > Filter.MaxSize) then
        MatchesFilter := False;
      
      // Verificar parâmetros
      if (Filter.MinParameters > 0) and (ModelInfo.Parameters < Filter.MinParameters) then
        MatchesFilter := False;
      if (Filter.MaxParameters > 0) and (ModelInfo.Parameters > Filter.MaxParameters) then
        MatchesFilter := False;
      
      // Verificar padrão do nome
      if (Filter.NamePattern <> '') and 
         not TRegEx.IsMatch(ModelInfo.Name, Filter.NamePattern, [roIgnoreCase]) then
        MatchesFilter := False;
      
      // Verificar se é custom
      if Filter.OnlyCustom and not ModelInfo.IsCustom then
        MatchesFilter := False;
      
      // Verificar tags
      if Length(Filter.Tags) > 0 then
      begin
        var HasTag := False;
        for I := 0 to High(Filter.Tags) do
        begin
          if Assigned(ModelInfo.Tags) and (ModelInfo.Tags.IndexOf(Filter.Tags[I]) >= 0) then
          begin
            HasTag := True;
            Break;
          end;
        end;
        if not HasTag then
          MatchesFilter := False;
      end;
      
      // Verificar data
      if (Filter.DateFrom > 0) and (ModelInfo.Created < Filter.DateFrom) then
        MatchesFilter := False;
      if (Filter.DateTo > 0) and (ModelInfo.Created > Filter.DateTo) then
        MatchesFilter := False;
      
      if MatchesFilter then
        FilteredModels.Add(ModelInfo);
    end;
    
    // Converter para array
    SetLength(Result, FilteredModels.Count);
    for I := 0 to FilteredModels.Count - 1 do
      Result[I] := FilteredModels[I];
      
  finally
    FilteredModels.Free;
  end;
end;

function TModelManager.SearchModels(const Query: string): TArray<TModelInfo>;
var
  SearchResults: TList<TModelInfo>;
  QueryLower: string;
  ModelInfo: TModelInfo;
  Score: Double;
  I: Integer;
begin
  SearchResults := TList<TModelInfo>.Create;
  try
    QueryLower := LowerCase(Query);
    
    for var Pair in FModels do
    begin
      ModelInfo := Pair.Value;
      Score := 0;
      
      // Buscar no nome (peso maior)
      if ContainsText(ModelInfo.Name, Query) then
        Score := Score + 10;
      
      // Buscar na descrição
      if ContainsText(ModelInfo.Description, Query) then
        Score := Score + 5;
      
      // Buscar em tags
      if Assigned(ModelInfo.Tags) then
      begin
        for I := 0 to ModelInfo.Tags.Count - 1 do
        begin
          if ContainsText(ModelInfo.Tags[I], Query) then
            Score := Score + 3;
        end;
      end;
      
      // Buscar em linguagens
      if ContainsText(ModelInfo.Languages, Query) then
        Score := Score + 2;
      
      // Buscar na família
      if ContainsText(ModelInfo.Family, Query) then
        Score := Score + 4;
      
      if Score > 0 then
        SearchResults.Add(ModelInfo);
    end;
    
    // Converter para array
    SetLength(Result, SearchResults.Count);
    for I := 0 to SearchResults.Count - 1 do
      Result[I] := SearchResults[I];
      
  finally
    SearchResults.Free;
  end;
end;

function TModelManager.BenchmarkModel(const ModelName: string; TestSuite: string): TModelBenchmark;
var
  StartTime: TDateTime;
  TestRequest: TGenerationRequest;
  Response: string;
  TokenCount: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestConfig := TJSONObject.Create;
  
  if not FBenchmarkEnabled then
  begin
    Result.ModelName := ModelName;
    Result.TestName := 'Benchmark Disabled';
    Exit;
  end;
  
  try
    Result.ModelName := ModelName;
    Result.TestName := TestSuite;
    Result.TestDate := Now;
    StartTime := Now;
    
    // Configurar teste baseado na suíte
    case LowerCase(TestSuite) of
      'standard':
      begin
        TestRequest.Model := ModelName;
        TestRequest.Prompt := 'Explique o conceito de programação orientada a objetos em 100 palavras.';
        TestRequest.Temperature := 0.7;
        TestRequest.MaxTokens := 200;
      end;
      
      'quick':
      begin
        TestRequest.Model := ModelName;
        TestRequest.Prompt := 'Olá, como você está?';
        TestRequest.Temperature := 0.5;
        TestRequest.MaxTokens := 50;
      end;
      
      'complex':
      begin
        TestRequest.Model := ModelName;
        TestRequest.Prompt := 'Escreva um algoritmo completo de ordenação quicksort em Pascal com explicações detalhadas.';
        TestRequest.Temperature := 0.3;
        TestRequest.MaxTokens := 1000;
      end;
      
    else
      TestRequest.Model := ModelName;
      TestRequest.Prompt := 'Teste de benchmark padrão.';
      TestRequest.Temperature := 0.7;
      TestRequest.MaxTokens := 100;
    end;
    
    // Executar teste
    Response := FOllamaAPI.Generate(TestRequest);
    Result.ExecutionTime := Now - StartTime;
    
    // Calcular métricas
    TokenCount := CountWords(Response) * 1.3; // Estimativa de tokens
    Result.TokensPerSecond := TokenCount / (Result.ExecutionTime * SecsPerDay);
    Result.MemoryUsage := GetProcessMemoryUsage; // Implementar
    
    // Calcular score baseado em velocidade e qualidade
    Result.Score := CalculateBenchmarkScore(Response, Result.ExecutionTime, TestSuite);
    
    // Adicionar configuração do teste
    Result.TestConfig.AddPair('prompt', TestRequest.Prompt);
    Result.TestConfig.AddPair('temperature', TJSONNumber.Create(TestRequest.Temperature));
    Result.TestConfig.AddPair('max_tokens', TJSONNumber.Create(TestRequest.MaxTokens));
    Result.TestConfig.AddPair('response_length', TJSONNumber.Create(Length(Response)));
    Result.TestConfig.AddPair('token_count', TJSONNumber.Create(TokenCount));
    
    // Armazenar benchmark
    FBenchmarks.Add(Result);
    
    if Assigned(FOnBenchmarkComplete) then
      FOnBenchmarkComplete(Self, Result);
      
  except
    on E: Exception do
    begin
      Result.Score := 0;
      Result.TestConfig.AddPair('error', E.Message);
      
      if Assigned(FOnError) then
        FOnError(Self, ModelName, 'Erro no benchmark: ' + E.Message);
    end;
  end;
end;

function TModelManager.CountWords(const Text: string): Integer;
var
  Words: TArray<string>;
begin
  Words := Text.Split([' ', #9, #10, #13], TStringSplitOptions.ExcludeEmpty);
  Result := Length(Words);
end;

function TModelManager.GetProcessMemoryUsage: Int64;
var
  MemInfo: TMemoryManagerState;
  SmallBlockInfo: TSmallBlockTypeState;
begin
  // Implementação básica - pode ser melhorada
  GetMemoryManagerState(MemInfo);
  Result := MemInfo.TotalAllocatedMediumBlockSize + MemInfo.TotalAllocatedLargeBlockSize;
  
  for var I := 0 to High(MemInfo.SmallBlockTypeStates) do
  begin
    SmallBlockInfo := MemInfo.SmallBlockTypeStates[I];
    Result := Result + SmallBlockInfo.UseableBlockSize * SmallBlockInfo.AllocatedBlockCount;
  end;
end;

function TModelManager.CalculateBenchmarkScore(const Response: string; 
  ExecutionTime: TDateTime; const TestSuite: string): Double;
var
  SpeedScore, QualityScore: Double;
  ResponseLength: Integer;
  ExecutionSeconds: Double;
begin
  ExecutionSeconds := ExecutionTime * SecsPerDay;
  ResponseLength := Length(Response);
  
  // Score de velocidade (0-50 pontos)
  if ExecutionSeconds < 1 then
    SpeedScore := 50
  else if ExecutionSeconds < 5 then
    SpeedScore := 40
  else if ExecutionSeconds < 10 then
    SpeedScore := 30
  else if ExecutionSeconds < 30 then
    SpeedScore := 20
  else
    SpeedScore := 10;
  
  // Score de qualidade baseado no comprimento e conteúdo (0-50 pontos)
  QualityScore := 0;
  
  if ResponseLength > 50 then
    QualityScore := QualityScore + 10;
  if ResponseLength > 100 then
    QualityScore := QualityScore + 10;
  if ResponseLength > 200 then
    QualityScore := QualityScore + 10;
    
  // Verificar qualidade do conteúdo
  if not Response.Contains('erro') and not Response.Contains('desculpe') then
    QualityScore := QualityScore + 10;
    
  if Response.Contains('.') and Response.Contains(' ') then // Frases completas
    QualityScore := QualityScore + 10;
  
  Result := SpeedScore + QualityScore;
  
  // Ajustar baseado na suíte de teste
  case LowerCase(TestSuite) of
    'quick': Result := Result * 0.8; // Testes rápidos valem menos
    'complex': Result := Result * 1.2; // Testes complexos valem mais
  end;
  
  Result := Min(100, Max(0, Result));
end;

// Implementações restantes dos métodos (versões simplificadas)

function TModelManager.IsValidModelName(const ModelName: string): Boolean;
begin
  Result := (Trim(ModelName) <> '') and not ModelName.Contains(' ') and 
            TRegEx.IsMatch(ModelName, '^[a-zA-Z0-9_\-\./:]+$');
end;

function TModelManager.GetModelSize(const ModelName: string): Int64;
var
  ModelInfo: TModelInfo;
begin
  Result := 0;
  if GetModelInfo(ModelName, ModelInfo) then
    Result := ModelInfo.Size;
end;

function TModelManager.GetModelDigest(const ModelName: string): string;
var
  ModelInfo: TModelInfo;
begin
  Result := '';
  if GetModelInfo(ModelName, ModelInfo) then
    Result := ModelInfo.Digest;
end;

function TModelManager.AnalyzeModelCapabilities(const ModelInfo: TModelInfo): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('supports_code', TJSONBool.Create(ContainsText(ModelInfo.Name, 'code')));
  Result.AddPair('supports_chat', TJSONBool.Create(ContainsText(ModelInfo.Name, 'chat')));
  Result.AddPair('supports_instruct', TJSONBool.Create(ContainsText(ModelInfo.Name, 'instruct')));
  Result.AddPair('parameter_count', TJSONNumber.Create(ModelInfo.Parameters));
  Result.AddPair('estimated_quality', TJSONNumber.Create(ModelInfo.Accuracy));
end;

function TModelManager.EstimateModelPerformance(const ModelInfo: TModelInfo): TJSONObject;
var
  Performance: Double;
begin
  Result := TJSONObject.Create;
  
  // Estimar performance baseado no tamanho
  if ModelInfo.Size < 1024 * 1024 * 1024 then // < 1GB
    Performance := 0.9 // Muito rápido
  else if ModelInfo.Size < 5 * 1024 * 1024 * 1024 then // < 5GB
    Performance := 0.7 // Rápido
  else if ModelInfo.Size < 20 * 1024 * 1024 * 1024 then // < 20GB
    Performance := 0.5 // Médio
  else
    Performance := 0.3; // Lento
  
  Result.AddPair('speed_estimate', TJSONNumber.Create(Performance));
  Result.AddPair('memory_requirement', TJSONNumber.Create(ModelInfo.Size * 1.5)); // 1.5x do tamanho
  Result.AddPair('cpu_intensive', TJSONBool.Create(ModelInfo.Size > 10 * 1024 * 1024 * 1024));
end;

procedure TModelManager.InitializeModelCategories;
begin
  FModelCategories.Add('llama', mcGeneral);
  FModelCategories.Add('codellama', mcCode);
  FModelCategories.Add('mistral', mcGeneral);
  FModelCategories.Add('vicuna', mcText);
  FModelCategories.Add('alpaca', mcText);
  FModelCategories.Add('llava', mcMultimodal);
  FModelCategories.Add('bakllava', mcMultimodal);
  FModelCategories.Add('custom', mcCustom);
end;

procedure TModelManager.LoadModelRecommendations;
var
  CodingRecommendations: TStringList;
  WritingRecommendations: TStringList;
  GeneralRecommendations: TStringList;
begin
  // Recomendações para coding
  CodingRecommendations := TStringList.Create;
  CodingRecommendations.Add('codellama');
  CodingRecommendations.Add('deepseek-coder');
  CodingRecommendations.Add('starcoder');
  FRecommendations.Add('coding', CodingRecommendations);
  
  // Recomendações para escrita
  WritingRecommendations := TStringList.Create;
  WritingRecommendations.Add('llama2');
  WritingRecommendations.Add('mistral');
  WritingRecommendations.Add('vicuna');
  FRecommendations.Add('writing', WritingRecommendations);
  
  // Recomendações gerais
  GeneralRecommendations := TStringList.Create;
  GeneralRecommendations.Add('llama2');
  GeneralRecommendations.Add('mistral');
  FRecommendations.Add('general', GeneralRecommendations);
end;

function TModelManager.ParseModelTags(const ModelName: string): TStringList;
begin
  Result := TStringList.Create;
  
  // Extrair tags do nome do modelo
  if ModelName.Contains('instruct') then
    Result.Add('instruct');
  if ModelName.Contains('chat') then
    Result.Add('chat');
  if ModelName.Contains('code') then
    Result.Add('code');
  if ModelName.Contains('7b') or ModelName.Contains('13b') or ModelName.Contains('70b') then
    Result.Add('quantized');
  if ModelName.Contains('uncensored') then
    Result.Add('uncensored');
end;

function TModelManager.ExtractModelFamily(const ModelName: string): string;
var
  FamilyPatterns: TArray<string>;
  I: Integer;
begin
  Result := 'unknown';
  
  FamilyPatterns := ['llama', 'mistral', 'vicuna', 'alpaca', 'codellama', 'llava'];
  
  for I := 0 to High(FamilyPatterns) do
  begin
    if ContainsText(ModelName, FamilyPatterns[I]) then
    begin
      Result := FamilyPatterns[I];
      Break;
    end;
  end;
end;

function TModelManager.GetAvailableSpace: Int64;
var
  FreeBytesAvailable, TotalBytes: Int64;
begin
  Result := 0;
  if GetDiskFreeSpaceEx(PChar(FDefaultDownloadPath), FreeBytesAvailable, TotalBytes, nil) then
    Result := FreeBytesAvailable;
end;

function TModelManager.ValidateModelIntegrity(const ModelName: string): Boolean;
begin
  // Implementação básica - verificar se modelo existe e responde
  Result := ModelExists(ModelName);
  
  if Result then
  begin
    try
      var TestRequest: TGenerationRequest;
      TestRequest.Model := ModelName;
      TestRequest.Prompt := 'test';
      TestRequest.MaxTokens := 10;
      
      var Response := FOllamaAPI.Generate(TestRequest);
      Result := (Response <> '') and not Response.Contains('erro');
    except
      Result := False;
    end;
  end;
end;

// Implementações placeholder para métodos restantes
function TModelManager.DownloadBatchModels(const ModelNames: TArray<string>): Boolean;
begin
  Result := True;
  for var ModelName in ModelNames do
    DownloadModel(ModelName);
end;

function TModelManager.GetDownloadProgress(const ModelName: string): Integer;
begin
  Result := 0; // Placeholder
end;

function TModelManager.CancelDownload(const ModelName: string): Boolean;
begin
  Result := False;
  if FDownloadTasks.ContainsKey(ModelName) then
  begin
    var Task := FDownloadTasks[ModelName];
    if Assigned(Task) then
    begin
      Task.Cancel;
      FDownloadTasks.Remove(ModelName);
      Result := True;
    end;
  end;
end;

function TModelManager.DeleteMultipleModels(const ModelNames: TArray<string>): Boolean;
begin
  Result := True;
  for var ModelName in ModelNames do
    Result := Result and DeleteModel(ModelName);
end;

function TModelManager.CleanupUnusedModels: Integer;
begin
  Result := 0; // Placeholder
end;

function TModelManager.GetModelsByCategory(Category: TModelCategory): TArray<TModelInfo>;
var
  Filter: TModelFilter;
begin
  FillChar(Filter, SizeOf(Filter), 0);
  Filter.Category := Category;
  Result := FilterModels(Filter);
end;

function TModelManager.GetModelStatistics: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('total_models', TJSONNumber.Create(FModels.Count));
  Result.AddPair('total_benchmarks', TJSONNumber.Create(FBenchmarks.Count));
  Result.AddPair('cache_size', TJSONNumber.Create(FModels.Count));
end;

function TModelManager.GenerateModelReport: string;
var
  Report: TStringBuilder;
begin
  Report := TStringBuilder.Create;
  try
    Report.AppendLine('=== RELATÓRIO DE MODELOS ===');
    Report.AppendLine('');
    Report.AppendLine('Total de modelos: ' + IntToStr(FModels.Count));
    Report.AppendLine('Benchmarks realizados: ' + IntToStr(FBenchmarks.Count));
    Report.AppendLine('Cache atualizado em: ' + DateTimeToStr(FCacheExpiry));
    
    Result := Report.ToString;
  finally
    Report.Free;
  end;
end;

// Implementações básicas para métodos restantes...
function TModelManager.GetModelRecommendations(const UseCase: string): TArray<string>;
var
  Recommendations: TStringList;
begin
  if FRecommendations.TryGetValue(LowerCase(UseCase), Recommendations) then
    Result := Recommendations.ToStringArray
  else
    SetLength(Result, 0);
end;

function TModelManager.BenchmarkAllModels(TestSuite: string): TArray<TModelBenchmark>;
var
  Benchmarks: TList<TModelBenchmark>;
begin
  Benchmarks := TList<TModelBenchmark>.Create;
  try
    for var ModelName in FModels.Keys do
      Benchmarks.Add(BenchmarkModel(ModelName, TestSuite));
    
    SetLength(Result, Benchmarks.Count);
    for var I := 0 to Benchmarks.Count - 1 do
      Result[I] := Benchmarks[I];
  finally
    Benchmarks.Free;
  end;
end;

procedure TModelManager.RefreshCache;
begin
  UpdateModelCache;
end;

procedure TModelManager.ClearCache;
begin
  for var Pair in FModels do
  begin
    if Assigned(Pair.Value.Tags) then
      Pair.Value.Tags.Free;
    if Assigned(Pair.Value.Performance) then
      Pair.Value.Performance.Free;
    if Assigned(Pair.Value.Capabilities) then
      Pair.Value.Capabilities.Free;
  end;
  FModels.Clear;
  FCacheExpiry := 0;
end;

// Adicionar implementações para métodos restantes conforme necessário...
function TModelManager.PauseDownload(const ModelName: string): Boolean; begin Result := False; end;
function TModelManager.ResumeDownload(const ModelName: string): Boolean; begin Result := False; end;
function TModelManager.CompactModelStorage: Boolean; begin Result := True; end;
function TModelManager.BackupModel(const ModelName, BackupPath: string): Boolean; begin Result := True; end;
function TModelManager.RestoreModel(const BackupPath, ModelName: string): Boolean; begin Result := True; end;
function TModelManager.GetModelsBySize(MinSize, MaxSize: Int64): TArray<TModelInfo>; begin SetLength(Result, 0); end;
function TModelManager.GetRecentModels(Days: Integer): TArray<TModelInfo>; begin SetLength(Result, 0); end;
function TModelManager.GetPopularModels: TArray<TModelInfo>; begin SetLength(Result, 0); end;
function TModelManager.SortModels(const Models: TArray<TModelInfo>; SortBy: string; Descending: Boolean): TArray<TModelInfo>; begin Result := Models; end;
function TModelManager.GetStorageStatistics: TJSONObject; begin Result := TJSONObject.Create; end;
function TModelManager.GetUsageStatistics: TJSONObject; begin Result := TJSONObject.Create; end;
function TModelManager.AnalyzeModelCollection: TJSONObject; begin Result := TJSONObject.Create; end;
function TModelManager.ExportModelList(const FileName: string; Format: string): Boolean; begin Result := True; end;
function TModelManager.ImportModelList(const FileName: string): Boolean; begin Result := True; end;

end.