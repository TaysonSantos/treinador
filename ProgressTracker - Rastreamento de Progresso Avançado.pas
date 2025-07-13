unit ProgressTracker;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.DateUtils, System.Math, System.Threading, System.SyncObjs,
  Vcl.Controls, Vcl.ComCtrls, Vcl.Forms, Vcl.Graphics, Vcl.ExtCtrls;

type
  TProgressStage = (psInitializing, psPreparingData, psTraining, psValidating, 
    psFineTuning, psTesting, psSaving, psCompleted, psError, psCancelled);
    
  TProgressPriority = (ppLow, ppNormal, ppHigh, ppCritical);
  
  TProgressStatus = (psIdle, psRunning, psPaused, psCompleted, psError, psCancelled);
  
  TProgressItem = record
    ID: string;
    Name: string;
    Description: string;
    Stage: TProgressStage;
    Status: TProgressStatus;
    Priority: TProgressPriority;
    Progress: Integer; // 0-100
    SubProgress: Integer; // 0-100 para sub-tarefas
    StartTime: TDateTime;
    EndTime: TDateTime;
    EstimatedTime: TDateTime;
    ElapsedTime: TDateTime;
    RemainingTime: TDateTime;
    CurrentStep: string;
    TotalSteps: Integer;
    CompletedSteps: Integer;
    Metadata: TJSONObject;
    ParentID: string;
    ChildIDs: TStringList;
    ErrorMessage: string;
    ErrorCode: Integer;
    RetryCount: Integer;
    MaxRetries: Integer;
    Throughput: Double; // items per second
    AverageSpeed: Double; // average processing speed
  end;
  
  TProgressSnapshot = record
    Timestamp: TDateTime;
    TotalProgress: Integer;
    ActiveTasks: Integer;
    CompletedTasks: Integer;
    ErrorTasks: Integer;
    MemoryUsage: Int64;
    CPUUsage: Double;
    DiskUsage: Int64;
    NetworkUsage: Int64;
    Metrics: TJSONObject;
  end;
  
  TProgressAlert = record
    ID: string;
    Type_: string; // warning, error, info, success
    Message: string;
    Timestamp: TDateTime;
    ProgressID: string;
    Severity: Integer; // 1-10
    AutoDismiss: Boolean;
    DismissAfter: Integer; // seconds
    Actions: TArray<string>;
  end;
  
  TProgressVisualization = record
    VisualizationType: string; // bar, circle, line, gauge
    Position: TPoint;
    Size: TSize;
    Colors: record
      Background: TColor;
      Foreground: TColor;
      Text: TColor;
      Border: TColor;
    end;
    Visible: Boolean;
    Animated: Boolean;
    ShowPercentage: Boolean;
    ShowTime: Boolean;
    ShowSpeed: Boolean;
    CustomFormat: string;
  end;
  
  TProgressNotification = record
    Type_: string; // email, sound, popup, webhook
    Enabled: Boolean;
    Settings: TJSONObject;
    Conditions: TJSONObject; // when to trigger
  end;
  
  TProgressEvent = procedure(Sender: TObject; const ProgressItem: TProgressItem) of object;
  TProgressCompleteEvent = procedure(Sender: TObject; const ProgressID: string; 
    Success: Boolean; ElapsedTime: TDateTime) of object;
  TProgressAlertEvent = procedure(Sender: TObject; const Alert: TProgressAlert) of object;
  TProgressStageChangeEvent = procedure(Sender: TObject; const ProgressID: string; 
    OldStage, NewStage: TProgressStage) of object;
    
  TProgressTracker = class
  private
    FProgressItems: TDictionary<string, TProgressItem>;
    FProgressHistory: TList<TProgressSnapshot>;
    FActiveAlerts: TList<TProgressAlert>;
    FNotifications: TArray<TProgressNotification>;
    FVisualizations: TDictionary<string, TProgressVisualization>;
    
    // Threading e sincronização
    FCriticalSection: TCriticalSection;
    FUpdateThread: TThread;
    FSnapshotThread: TThread;
    FShutdownEvent: TEvent;
    
    // Configurações
    FEnabled: Boolean;
    FUpdateInterval: Integer; // milliseconds
    FSnapshotInterval: Integer; // seconds
    FMaxHistoryItems: Integer;
    FAutoCleanup: Boolean;
    FCleanupAfterDays: Integer;
    FEnableDetailedLogging: Boolean;
    FEnablePredictiveAnalysis: Boolean;
    FEnableRealTimeUpdates: Boolean;
    
    // UI Components
    FProgressBars: TDictionary<string, TProgressBar>;
    FProgressLabels: TDictionary<string, TLabel>;
    FProgressPanels: TDictionary<string, TPanel>;
    FOwnerControl: TWinControl;
    
    // Métricas e análise
    FPerformanceMetrics: TDictionary<string, TJSONObject>;
    FPredictionModel: TJSONObject;
    FBenchmarkData: TDictionary<string, TArray<Double>>;
    
    // Eventos
    FOnProgress: TProgressEvent;
    FOnComplete: TProgressCompleteEvent;
    FOnAlert: TProgressAlertEvent;
    FOnStageChange: TProgressStageChangeEvent;
    
    function GenerateProgressID: string;
    function CalculateETA(const ProgressItem: TProgressItem): TDateTime;
    function CalculateSpeed(const ProgressItem: TProgressItem): Double;
    function EstimateRemainingTime(const ProgressItem: TProgressItem): TDateTime;
    procedure UpdateVisualization(const ProgressID: string);
    procedure CreateProgressBar(const ProgressID: string; Parent: TWinControl);
    procedure UpdateProgressBar(const ProgressID: string; const ProgressItem: TProgressItem);
    procedure RemoveProgressBar(const ProgressID: string);
    procedure TakeSnapshot;
    procedure CleanupOldData;
    procedure ProcessAlerts;
    procedure SendNotification(const ProgressItem: TProgressItem; const NotificationType: string);
    procedure UpdateThreadExecute;
    procedure SnapshotThreadExecute;
    function PredictCompletion(const ProgressItem: TProgressItem): TDateTime;
    function AnalyzePerformance(const ProgressID: string): TJSONObject;
    procedure RecordBenchmark(const ProgressID: string; const Metric: string; const Value: Double);
    function GetAveragePerformance(const MetricName: string): Double;
    procedure ValidateProgressItem(var ProgressItem: TProgressItem);
    function ShouldTriggerAlert(const ProgressItem: TProgressItem): Boolean;
    procedure CreateAlert(const ProgressID, AlertType, Message: string; Severity: Integer = 5);
    procedure DismissAlert(const AlertID: string);
    function FormatTime(const Time: TDateTime): string;
    function FormatSpeed(const Speed: Double): string;
    function FormatProgress(const ProgressItem: TProgressItem): string;
    
  public
    constructor Create(OwnerControl: TWinControl = nil);
    destructor Destroy; override;
    
    // Propriedades
    property Enabled: Boolean read FEnabled write FEnabled;
    property UpdateInterval: Integer read FUpdateInterval write FUpdateInterval;
    property SnapshotInterval: Integer read FSnapshotInterval write FSnapshotInterval;
    property MaxHistoryItems: Integer read FMaxHistoryItems write FMaxHistoryItems;
    property AutoCleanup: Boolean read FAutoCleanup write FAutoCleanup;
    property EnableDetailedLogging: Boolean read FEnableDetailedLogging write FEnableDetailedLogging;
    property EnablePredictiveAnalysis: Boolean read FEnablePredictiveAnalysis write FEnablePredictiveAnalysis;
    property EnableRealTimeUpdates: Boolean read FEnableRealTimeUpdates write FEnableRealTimeUpdates;
    
    // Eventos
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TProgressCompleteEvent read FOnComplete write FOnComplete;
    property OnAlert: TProgressAlertEvent read FOnAlert write FOnAlert;
    property OnStageChange: TProgressStageChangeEvent read FOnStageChange write FOnStageChange;
    
    // Gerenciamento de progresso
    function StartProgress(const Name, Description: string; TotalSteps: Integer = 100; 
      Stage: TProgressStage = psInitializing; Priority: TProgressPriority = ppNormal): string;
    function StartSubProgress(const ParentID, Name, Description: string; 
      TotalSteps: Integer = 100): string;
    procedure UpdateProgress(const ProgressID: string; Progress: Integer; 
      const CurrentStep: string = ''; const Metadata: TJSONObject = nil);
    procedure UpdateSubProgress(const ProgressID: string; SubProgress: Integer; 
      const CurrentStep: string = '');
    procedure SetProgressStage(const ProgressID: string; Stage: TProgressStage);
    procedure SetProgressStatus(const ProgressID: string; Status: TProgressStatus);
    procedure CompleteProgress(const ProgressID: string; Success: Boolean = True; 
      const Message: string = '');
    procedure CancelProgress(const ProgressID: string; const Reason: string = '');
    procedure PauseProgress(const ProgressID: string);
    procedure ResumeProgress(const ProgressID: string);
    procedure RetryProgress(const ProgressID: string);
    
    // Consulta e análise
    function GetProgress(const ProgressID: string): TProgressItem;
    function GetActiveProgresses: TArray<TProgressItem>;
    function GetProgressesByStage(Stage: TProgressStage): TArray<TProgressItem>;
    function GetProgressesByStatus(Status: TProgressStatus): TArray<TProgressItem>;
    function GetProgressesByPriority(Priority: TProgressPriority): TArray<TProgressItem>;
    function GetProgressHistory(const ProgressID: string): TArray<TProgressSnapshot>;
    function GetOverallProgress: Integer;
    function GetActiveTasksCount: Integer;
    function GetCompletedTasksCount: Integer;
    function GetErrorTasksCount: Integer;
    function GetAverageCompletionTime: TDateTime;
    function GetTotalElapsedTime: TDateTime;
    function GetEstimatedRemainingTime: TDateTime;
    
    // Visualização
    procedure CreateVisualization(const ProgressID: string; 
      const Visualization: TProgressVisualization);
    procedure UpdateVisualization(const ProgressID: string; 
      const Visualization: TProgressVisualization);
    procedure RemoveVisualization(const ProgressID: string);
    procedure ShowProgressDialog(const ProgressID: string; Modal: Boolean = False);
    procedure HideProgressDialog(const ProgressID: string);
    procedure RefreshAllVisualizations;
    function CreateProgressWindow(const ProgressID: string; 
      const WindowTitle: string = ''): TForm;
    
    // Alertas e notificações
    function GetActiveAlerts: TArray<TProgressAlert>;
    function GetAlertsByType(const AlertType: string): TArray<TProgressAlert>;
    procedure DismissAllAlerts;
    procedure ConfigureNotification(const NotificationType: string; 
      const Settings: TJSONObject);
    procedure EnableNotification(const NotificationType: string; Enabled: Boolean = True);
    function TestNotification(const NotificationType: string): Boolean;
    
    // Estatísticas e relatórios
    function GetStatistics: TJSONObject;
    function GetPerformanceReport: string;
    function GetProgressReport(const ProgressID: string): string;
    function ExportProgressData(const FileName: string; Format: string = 'json'): Boolean;
    function ImportProgressData(const FileName: string): Boolean;
    function GenerateChart(const MetricName: string; const TimeRange: Integer = 24): string;
    function AnalyzeTrends(const MetricName: string; Days: Integer = 7): TJSONObject;
    
    // Benchmarking e performance
    procedure StartBenchmark(const BenchmarkName: string);
    procedure EndBenchmark(const BenchmarkName: string);
    function GetBenchmarkResults(const BenchmarkName: string): TJSONObject;
    procedure CompareBenchmarks(const Benchmark1, Benchmark2: string);
    function OptimizePerformance: TStringList;
    function DetectBottlenecks: TArray<string>;
    
    // Configuração e personalização
    procedure LoadConfiguration(const ConfigFile: string);
    procedure SaveConfiguration(const ConfigFile: string);
    procedure SetTheme(const ThemeName: string);
    procedure CustomizeVisualization(const ProgressID: string; 
      const CustomSettings: TJSONObject);
    procedure SetUpdateFrequency(const ProgressID: string; FrequencyMs: Integer);
    procedure EnableDetailedTracking(const ProgressID: string; Enabled: Boolean = True);
    
    // Integração e extensibilidade
    function RegisterCustomStage(const StageName: string; const StageColor: TColor): Boolean;
    function RegisterCustomMetric(const MetricName: string; 
      const CalculationFunction: TFunc<TProgressItem, Double>): Boolean;
    procedure AddCustomAction(const ProgressID, ActionName: string; 
      const ActionProc: TProc<string>);
    function CreateProgressTemplate(const TemplateName: string; 
      const Template: TProgressItem): Boolean;
    function ApplyProgressTemplate(const TemplateName: string): string;
    
    // Utilidades
    procedure Clear;
    procedure Reset;
    function Validate: TArray<string>;
    function GetMemoryUsage: Int64;
    function GetCPUUsage: Double;
    function IsHealthy: Boolean;
    procedure Cleanup;
    procedure Optimize;
    function GetDebugInfo: TJSONObject;
  end;

var
  GlobalProgressTracker: TProgressTracker;

// Funções de conveniência globais
function StartProgress(const Name, Description: string; TotalSteps: Integer = 100): string;
procedure UpdateProgress(const ProgressID: string; Progress: Integer; const CurrentStep: string = '');
procedure CompleteProgress(const ProgressID: string; Success: Boolean = True);
procedure SetProgressStage(const ProgressID: string; Stage: TProgressStage);

implementation

uses
  System.Variants, System.Hash, System.RegularExpressions, System.IOUtils,
  Winapi.Windows, Winapi.PsAPI, Vcl.Dialogs;

{ TProgressTracker }

constructor TProgressTracker.Create(OwnerControl: TWinControl);
begin
  inherited Create;
  
  FOwnerControl := OwnerControl;
  FProgressItems := TDictionary<string, TProgressItem>.Create;
  FProgressHistory := TList<TProgressSnapshot>.Create;
  FActiveAlerts := TList<TProgressAlert>.Create;
  FVisualizations := TDictionary<string, TProgressVisualization>.Create;
  FProgressBars := TDictionary<string, TProgressBar>.Create;
  FProgressLabels := TDictionary<string, TLabel>.Create;
  FProgressPanels := TDictionary<string, TPanel>.Create;
  FPerformanceMetrics := TDictionary<string, TJSONObject>.Create;
  FBenchmarkData := TDictionary<string, TArray<Double>>.Create;
  
  FCriticalSection := TCriticalSection.Create;
  FShutdownEvent := TEvent.Create(nil, True, False, '');
  
  // Configurações padrão
  FEnabled := True;
  FUpdateInterval := 100; // 100ms
  FSnapshotInterval := 5; // 5 segundos
  FMaxHistoryItems := 1000;
  FAutoCleanup := True;
  FCleanupAfterDays := 7;
  FEnableDetailedLogging := False;
  FEnablePredictiveAnalysis := True;
  FEnableRealTimeUpdates := True;
  
  FPredictionModel := TJSONObject.Create;
  
  // Inicializar threads de atualização
  if FEnableRealTimeUpdates then
  begin
    FUpdateThread := TThread.CreateAnonymousThread(UpdateThreadExecute);
    FUpdateThread.Start;
    
    FSnapshotThread := TThread.CreateAnonymousThread(SnapshotThreadExecute);
    FSnapshotThread.Start;
  end;
end;

destructor TProgressTracker.Destroy;
var
  ProgressItem: TProgressItem;
  Alert: TProgressAlert;
  Visualization: TProgressVisualization;
begin
  // Sinalizar shutdown
  FShutdownEvent.SetEvent;
  
  // Aguardar threads terminarem
  if Assigned(FUpdateThread) then
  begin
    FUpdateThread.WaitFor;
    FUpdateThread.Free;
  end;
  
  if Assigned(FSnapshotThread) then
  begin
    FSnapshotThread.WaitFor;
    FSnapshotThread.Free;
  end;
  
  // Limpar recursos dos itens de progresso
  for var Pair in FProgressItems do
  begin
    ProgressItem := Pair.Value;
    if Assigned(ProgressItem.Metadata) then
      ProgressItem.Metadata.Free;
    if Assigned(ProgressItem.ChildIDs) then
      ProgressItem.ChildIDs.Free;
  end;
  
  // Limpar alertas
  for var I := 0 to FActiveAlerts.Count - 1 do
  begin
    Alert := FActiveAlerts[I];
    // Liberar recursos se necessário
  end;
  
  // Limpar histórico de snapshots
  for var I := 0 to FProgressHistory.Count - 1 do
  begin
    var Snapshot := FProgressHistory[I];
    if Assigned(Snapshot.Metrics) then
      Snapshot.Metrics.Free;
  end;
  
  // Limpar métricas de performance
  for var Pair in FPerformanceMetrics do
    Pair.Value.Free;
  
  // Limpar componentes visuais
  for var Pair in FProgressBars do
    Pair.Value.Free;
  for var Pair in FProgressLabels do
    Pair.Value.Free;
  for var Pair in FProgressPanels do
    Pair.Value.Free;
  
  FPredictionModel.Free;
  FBenchmarkData.Free;
  FPerformanceMetrics.Free;
  FProgressPanels.Free;
  FProgressLabels.Free;
  FProgressBars.Free;
  FVisualizations.Free;
  FActiveAlerts.Free;
  FProgressHistory.Free;
  FProgressItems.Free;
  FShutdownEvent.Free;
  FCriticalSection.Free;
  
  inherited Destroy;
end;

function TProgressTracker.StartProgress(const Name, Description: string; 
  TotalSteps: Integer; Stage: TProgressStage; Priority: TProgressPriority): string;
var
  ProgressItem: TProgressItem;
begin
  Result := GenerateProgressID;
  
  FCriticalSection.Acquire;
  try
    // Inicializar item de progresso
    FillChar(ProgressItem, SizeOf(ProgressItem), 0);
    ProgressItem.ID := Result;
    ProgressItem.Name := Name;
    ProgressItem.Description := Description;
    ProgressItem.Stage := Stage;
    ProgressItem.Status := psRunning;
    ProgressItem.Priority := Priority;
    ProgressItem.Progress := 0;
    ProgressItem.SubProgress := 0;
    ProgressItem.StartTime := Now;
    ProgressItem.TotalSteps := TotalSteps;
    ProgressItem.CompletedSteps := 0;
    ProgressItem.CurrentStep := 'Iniciando...';
    ProgressItem.Metadata := TJSONObject.Create;
    ProgressItem.ChildIDs := TStringList.Create;
    ProgressItem.ParentID := '';
    ProgressItem.ErrorMessage := '';
    ProgressItem.ErrorCode := 0;
    ProgressItem.RetryCount := 0;
    ProgressItem.MaxRetries := 3;
    ProgressItem.Throughput := 0;
    ProgressItem.AverageSpeed := 0;
    
    // Adicionar metadados iniciais
    ProgressItem.Metadata.AddPair('created_at', DateToISO8601(Now));
    ProgressItem.Metadata.AddPair('created_by', 'ProgressTracker');
    ProgressItem.Metadata.AddPair('version', '2.0');
    
    ValidateProgressItem(ProgressItem);
    FProgressItems.Add(Result, ProgressItem);
    
    // Criar visualização se owner control estiver disponível
    if Assigned(FOwnerControl) then
      CreateProgressBar(Result, FOwnerControl);
    
  finally
    FCriticalSection.Release;
  end;
  
  // Disparar evento
  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressItem);
  
  // Log detalhado se habilitado
  if FEnableDetailedLogging then
  begin
    var LogData := TJSONObject.Create;
    try
      LogData.AddPair('action', 'start_progress');
      LogData.AddPair('progress_id', Result);
      LogData.AddPair('name', Name);
      LogData.AddPair('description', Description);
      LogData.AddPair('total_steps', TJSONNumber.Create(TotalSteps));
      // Log seria enviado para o sistema de logging aqui
    finally
      LogData.Free;
    end;
  end;
end;

procedure TProgressTracker.UpdateProgress(const ProgressID: string; Progress: Integer; 
  const CurrentStep: string; const Metadata: TJSONObject);
var
  ProgressItem: TProgressItem;
  OldProgress: Integer;
begin
  FCriticalSection.Acquire;
  try
    if not FProgressItems.TryGetValue(ProgressID, ProgressItem) then
      Exit;
    
    OldProgress := ProgressItem.Progress;
    
    // Atualizar progresso
    ProgressItem.Progress := Max(0, Min(100, Progress));
    ProgressItem.CompletedSteps := Round((ProgressItem.Progress / 100) * ProgressItem.TotalSteps);
    
    if CurrentStep <> '' then
      ProgressItem.CurrentStep := CurrentStep;
    
    // Atualizar metadados se fornecidos
    if Assigned(Metadata) then
    begin
      for var Pair in Metadata do
        ProgressItem.Metadata.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    end;
    
    // Calcular métricas de performance
    ProgressItem.ElapsedTime := Now - ProgressItem.StartTime;
    ProgressItem.AverageSpeed := CalculateSpeed(ProgressItem);
    ProgressItem.RemainingTime := EstimateRemainingTime(ProgressItem);
    
    if FEnablePredictiveAnalysis then
      ProgressItem.EstimatedTime := PredictCompletion(ProgressItem);
    
    // Atualizar throughput (items por segundo)
    if ProgressItem.ElapsedTime > 0 then
      ProgressItem.Throughput := ProgressItem.CompletedSteps / (ProgressItem.ElapsedTime * SecsPerDay);
    
    // Atualizar no dicionário
    FProgressItems.AddOrSetValue(ProgressID, ProgressItem);
    
    // Atualizar visualização
    if FEnableRealTimeUpdates then
      UpdateVisualization(ProgressID);
    
    // Verificar se deve criar alerta
    if ShouldTriggerAlert(ProgressItem) then
    begin
      if ProgressItem.Progress > OldProgress + 10 then // Progresso rápido
        CreateAlert(ProgressID, 'info', 'Progresso acelerado detectado', 3)
      else if (Now - ProgressItem.StartTime) > (1/24) and ProgressItem.Progress < 10 then // Progresso lento
        CreateAlert(ProgressID, 'warning', 'Progresso mais lento que o esperado', 6);
    end;
    
  finally
    FCriticalSection.Release;
  end;
  
  // Disparar evento
  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressItem);
end;

procedure TProgressTracker.CompleteProgress(const ProgressID: string; Success: Boolean; const Message: string);
var
  ProgressItem: TProgressItem;
  ElapsedTime: TDateTime;
begin
  FCriticalSection.Acquire;
  try
    if not FProgressItems.TryGetValue(ProgressID, ProgressItem) then
      Exit;
    
    // Atualizar status final
    ProgressItem.Progress := 100;
    ProgressItem.CompletedSteps := ProgressItem.TotalSteps;
    ProgressItem.EndTime := Now;
    ProgressItem.ElapsedTime := ProgressItem.EndTime - ProgressItem.StartTime;
    
    if Success then
    begin
      ProgressItem.Status := psCompleted;
      ProgressItem.CurrentStep := Message <> '' ? Message : 'Concluído com sucesso';
    end
    else
    begin
      ProgressItem.Status := psError;
      ProgressItem.ErrorMessage := Message;
      ProgressItem.CurrentStep := 'Erro: ' + Message;
    end;
    
    // Atualizar metadados finais
    ProgressItem.Metadata.AddPair('completed_at', DateToISO8601(Now));
    ProgressItem.Metadata.AddPair('success', TJSONBool.Create(Success));
    ProgressItem.Metadata.AddPair('elapsed_time_seconds', 
      TJSONNumber.Create(ProgressItem.ElapsedTime * SecsPerDay));
    
    if Message <> '' then
      ProgressItem.Metadata.AddPair('completion_message', Message);
    
    FProgressItems.AddOrSetValue(ProgressID, ProgressItem);
    
    // Atualizar visualização final
    UpdateVisualization(ProgressID);
    
    // Registrar benchmark se necessário
    RecordBenchmark(ProgressID, 'completion_time', ProgressItem.ElapsedTime * SecsPerDay);
    
    // Completar sub-progressos se existirem
    for var ChildID in ProgressItem.ChildIDs do
    begin
      var ChildItem: TProgressItem;
      if FProgressItems.TryGetValue(ChildID, ChildItem) and 
         (ChildItem.Status = psRunning) then
        CompleteProgress(ChildID, Success, 'Completado pelo pai');
    end;
    
  finally
    FCriticalSection.Release;
  end;
  
  ElapsedTime := ProgressItem.ElapsedTime;
  
  // Disparar evento de conclusão
  if Assigned(FOnComplete) then
    FOnComplete(Self, ProgressID, Success, ElapsedTime);
  
  // Enviar notificação se configurado
  if Success then
    SendNotification(ProgressItem, 'completion')
  else
    SendNotification(ProgressItem, 'error');
    
  // Criar alerta de conclusão
  if Success then
    CreateAlert(ProgressID, 'success', 
      Format('Tarefa "%s" concluída em %s', [ProgressItem.Name, FormatTime(ElapsedTime)]), 2)
  else
    CreateAlert(ProgressID, 'error', 
      Format('Tarefa "%s" falhou: %s', [ProgressItem.Name, Message]), 8);
end;

procedure TProgressTracker.SetProgressStage(const ProgressID: string; Stage: TProgressStage);
var
  ProgressItem: TProgressItem;
  OldStage: TProgressStage;
begin
  FCriticalSection.Acquire;
  try
    if not FProgressItems.TryGetValue(ProgressID, ProgressItem) then
      Exit;
    
    OldStage := ProgressItem.Stage;
    ProgressItem.Stage := Stage;
    
    // Atualizar step baseado no estágio
    case Stage of
      psInitializing: ProgressItem.CurrentStep := 'Inicializando...';
      psPreparingData: ProgressItem.CurrentStep := 'Preparando dados...';
      psTraining: ProgressItem.CurrentStep := 'Treinando modelo...';
      psValidating: ProgressItem.CurrentStep := 'Validando resultados...';
      psFineTuning: ProgressItem.CurrentStep := 'Ajustando parâmetros...';
      psTesting: ProgressItem.CurrentStep := 'Testando modelo...';
      psSaving: ProgressItem.CurrentStep := 'Salvando resultados...';
      psCompleted: ProgressItem.CurrentStep := 'Processo concluído';
      psError: ProgressItem.CurrentStep := 'Erro no processamento';
      psCancelled: ProgressItem.CurrentStep := 'Processo cancelado';
    end;
    
    ProgressItem.Metadata.AddPair('stage_changed_at', DateToISO8601(Now));
    ProgressItem.Metadata.AddPair('previous_stage', GetEnumName(TypeInfo(TProgressStage), Ord(OldStage)));
    ProgressItem.Metadata.AddPair('current_stage', GetEnumName(TypeInfo(TProgressStage), Ord(Stage)));
    
    FProgressItems.AddOrSetValue(ProgressID, ProgressItem);
    
  finally
    FCriticalSection.Release;
  end;
  
  // Disparar evento de mudança de estágio
  if Assigned(FOnStageChange) then
    FOnStageChange(Self, ProgressID, OldStage, Stage);
  
  // Atualizar visualização
  UpdateVisualization(ProgressID);
  
  // Log da mudança de estágio
  if FEnableDetailedLogging then
  begin
    CreateAlert(ProgressID, 'info', 
      Format('Estágio alterado de %s para %s', 
        [GetEnumName(TypeInfo(TProgressStage), Ord(OldStage)),
         GetEnumName(TypeInfo(TProgressStage), Ord(Stage))]), 3);
  end;
end;

function TProgressTracker.GenerateProgressID: string;
begin
  Result := 'progress_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + 
    IntToStr(Random(1000)).PadLeft(3, '0');
end;

function TProgressTracker.CalculateETA(const ProgressItem: TProgressItem): TDateTime;
var
  ElapsedTime: TDateTime;
  CompletionRate: Double;
begin
  Result := 0;
  
  if ProgressItem.Progress <= 0 then
    Exit;
  
  ElapsedTime := Now - ProgressItem.StartTime;
  CompletionRate := ProgressItem.Progress / 100.0;
  
  if CompletionRate > 0 then
    Result := ElapsedTime / CompletionRate - ElapsedTime;
end;

function TProgressTracker.CalculateSpeed(const ProgressItem: TProgressItem): Double;
var
  ElapsedSeconds: Double;
begin
  Result := 0;
  
  ElapsedSeconds := (Now - ProgressItem.StartTime) * SecsPerDay;
  if ElapsedSeconds > 0 then
    Result := ProgressItem.CompletedSteps / ElapsedSeconds;
end;

function TProgressTracker.EstimateRemainingTime(const ProgressItem: TProgressItem): TDateTime;
var
  RemainingSteps: Integer;
  EstimatedSeconds: Double;
begin
  Result := 0;
  
  if ProgressItem.AverageSpeed <= 0 then
    Exit;
  
  RemainingSteps := ProgressItem.TotalSteps - ProgressItem.CompletedSteps;
  EstimatedSeconds := RemainingSteps / ProgressItem.AverageSpeed;
  Result := EstimatedSeconds / SecsPerDay;
end;

procedure TProgressTracker.UpdateVisualization(const ProgressID: string);
var
  ProgressItem: TProgressItem;
  ProgressBar: TProgressBar;
  ProgressLabel: TLabel;
begin
  if not FEnableRealTimeUpdates then
    Exit;
  
  FCriticalSection.Acquire;
  try
    if not FProgressItems.TryGetValue(ProgressID, ProgressItem) then
      Exit;
  finally
    FCriticalSection.Release;
  end;
  
  // Atualizar barra de progresso
  if FProgressBars.TryGetValue(ProgressID, ProgressBar) then
  begin
    ProgressBar.Position := ProgressItem.Progress;
    
    // Atualizar cor baseado no status
    case ProgressItem.Status of
      psRunning: ProgressBar.BarColor := clBlue;
      psCompleted: ProgressBar.BarColor := clGreen;
      psError: ProgressBar.BarColor := clRed;
      psPaused: ProgressBar.BarColor := clYellow;
      psCancelled: ProgressBar.BarColor := clGray;
    end;
  end;
  
  // Atualizar label
  if FProgressLabels.TryGetValue(ProgressID, ProgressLabel) then
  begin
    ProgressLabel.Caption := FormatProgress(ProgressItem);
  end;
end;

procedure TProgressTracker.CreateProgressBar(const ProgressID: string; Parent: TWinControl);
var
  ProgressBar: TProgressBar;
  ProgressLabel: TLabel;
  ProgressPanel: TPanel;
  BarCount: Integer;
begin
  if not Assigned(Parent) then
    Exit;
  
  BarCount := FProgressBars.Count;
  
  // Criar panel container
  ProgressPanel := TPanel.Create(Parent);
  ProgressPanel.Parent := Parent;
  ProgressPanel.Height := 50;
  ProgressPanel.Top := BarCount * 55;
  ProgressPanel.Align := alTop;
  ProgressPanel.BevelOuter := bvNone;
  ProgressPanel.Color := clWindow;
  FProgressPanels.Add(ProgressID, ProgressPanel);
  
  // Criar label
  ProgressLabel := TLabel.Create(ProgressPanel);
  ProgressLabel.Parent := ProgressPanel;
  ProgressLabel.Left := 8;
  ProgressLabel.Top := 8;
  ProgressLabel.Caption := 'Iniciando...';
  ProgressLabel.Font.Style := [fsBold];
  FProgressLabels.Add(ProgressID, ProgressLabel);
  
  // Criar barra de progresso
  ProgressBar := TProgressBar.Create(ProgressPanel);
  ProgressBar.Parent := ProgressPanel;
  ProgressBar.Left := 8;
  ProgressBar.Top := 25;
  ProgressBar.Width := ProgressPanel.Width - 16;
  ProgressBar.Height := 18;
  ProgressBar.Anchors := [akLeft, akTop, akRight];
  ProgressBar.Min := 0;
  ProgressBar.Max := 100;
  ProgressBar.Position := 0;
  ProgressBar.Smooth := True;
  FProgressBars.Add(ProgressID, ProgressBar);
end;

procedure TProgressTracker.TakeSnapshot;
var
  Snapshot: TProgressSnapshot;
  ActiveTasks, CompletedTasks, ErrorTasks: Integer;
  TotalProgress: Double;
  ProgressItem: TProgressItem;
begin
  FCriticalSection.Acquire;
  try
    Snapshot.Timestamp := Now;
    ActiveTasks := 0;
    CompletedTasks := 0;
    ErrorTasks := 0;
    TotalProgress := 0;
    
    for var Pair in FProgressItems do
    begin
      ProgressItem := Pair.Value;
      
      case ProgressItem.Status of
        psRunning, psPaused: Inc(ActiveTasks);
        psCompleted: Inc(CompletedTasks);
        psError, psCancelled: Inc(ErrorTasks);
      end;
      
      TotalProgress := TotalProgress + ProgressItem.Progress;
    end;
    
    if FProgressItems.Count > 0 then
      Snapshot.TotalProgress := Round(TotalProgress / FProgressItems.Count)
    else
      Snapshot.TotalProgress := 0;
    
    Snapshot.ActiveTasks := ActiveTasks;
    Snapshot.CompletedTasks := CompletedTasks;
    Snapshot.ErrorTasks := ErrorTasks;
    Snapshot.MemoryUsage := GetMemoryUsage;
    Snapshot.CPUUsage := GetCPUUsage;
    Snapshot.DiskUsage := 0; // Implementar se necessário
    Snapshot.NetworkUsage := 0; // Implementar se necessário
    
    // Criar métricas do snapshot
    Snapshot.Metrics := TJSONObject.Create;
    Snapshot.Metrics.AddPair('total_tasks', TJSONNumber.Create(FProgressItems.Count));
    Snapshot.Metrics.AddPair('active_tasks', TJSONNumber.Create(ActiveTasks));
    Snapshot.Metrics.AddPair('completed_tasks', TJSONNumber.Create(CompletedTasks));
    Snapshot.Metrics.AddPair('error_tasks', TJSONNumber.Create(ErrorTasks));
    Snapshot.Metrics.AddPair('average_progress', TJSONNumber.Create(Snapshot.TotalProgress));
    Snapshot.Metrics.AddPair('memory_usage_mb', TJSONNumber.Create(Snapshot.MemoryUsage div (1024*1024)));
    Snapshot.Metrics.AddPair('cpu_usage_percent', TJSONNumber.Create(Snapshot.CPUUsage));
    
    FProgressHistory.Add(Snapshot);
    
    // Limitar histórico
    while FProgressHistory.Count > FMaxHistoryItems do
    begin
      var OldSnapshot := FProgressHistory[0];
      if Assigned(OldSnapshot.Metrics) then
        OldSnapshot.Metrics.Free;
      FProgressHistory.Delete(0);
    end;
    
  finally
    FCriticalSection.Release;
  end;
end;

procedure TProgressTracker.UpdateThreadExecute;
begin
  while not FShutdownEvent.WaitFor(FUpdateInterval) do
  begin
    if not FEnabled then
      Continue;
    
    try
      // Atualizar todas as visualizações
      if FEnableRealTimeUpdates then
        RefreshAllVisualizations;
      
      // Processar alertas
      ProcessAlerts;
      
      // Cleanup automático se habilitado
      if FAutoCleanup and (Random(100) = 0) then // 1% de chance por ciclo
        CleanupOldData;
        
    except
      // Log erro se sistema de logging estiver disponível
    end;
  end;
end;

procedure TProgressTracker.SnapshotThreadExecute;
begin
  while not FShutdownEvent.WaitFor(FSnapshotInterval * 1000) do
  begin
    if not FEnabled then
      Continue;
    
    try
      TakeSnapshot;
    except
      // Log erro se sistema de logging estiver disponível
    end;
  end;
end;

// Implementações de métodos auxiliares

function TProgressTracker.FormatTime(const Time: TDateTime): string;
var
  Hours, Minutes, Seconds: Integer;
  TotalSeconds: Integer;
begin
  TotalSeconds := Round(Time * SecsPerDay);
  Hours := TotalSeconds div 3600;
  Minutes := (TotalSeconds mod 3600) div 60;
  Seconds := TotalSeconds mod 60;
  
  if Hours > 0 then
    Result := Format('%d:%02d:%02d', [Hours, Minutes, Seconds])
  else
    Result := Format('%d:%02d', [Minutes, Seconds]);
end;

function TProgressTracker.FormatSpeed(const Speed: Double): string;
begin
  if Speed >= 1000 then
    Result := Format('%.1fk/s', [Speed / 1000])
  else if Speed >= 1 then
    Result := Format('%.1f/s', [Speed])
  else
    Result := Format('%.2f/s', [Speed]);
end;

function TProgressTracker.FormatProgress(const ProgressItem: TProgressItem): string;
begin
  Result := Format('%s - %d%% (%d/%d)', 
    [ProgressItem.Name, ProgressItem.Progress, ProgressItem.CompletedSteps, ProgressItem.TotalSteps]);
    
  if ProgressItem.AverageSpeed > 0 then
    Result := Result + Format(' - %s', [FormatSpeed(ProgressItem.AverageSpeed)]);
    
  if ProgressItem.RemainingTime > 0 then
    Result := Result + Format(' - ETA: %s', [FormatTime(ProgressItem.RemainingTime)]);
end;

// Implementações básicas para métodos restantes

function TProgressTracker.GetProgress(const ProgressID: string): TProgressItem;
begin
  FCriticalSection.Acquire;
  try
    if not FProgressItems.TryGetValue(ProgressID, Result) then
      FillChar(Result, SizeOf(Result), 0);
  finally
    FCriticalSection.Release;
  end;
end;

function TProgressTracker.GetActiveProgresses: TArray<TProgressItem>;
var
  ActiveList: TList<TProgressItem>;
begin
  ActiveList := TList<TProgressItem>.Create;
  try
    FCriticalSection.Acquire;
    try
      for var Pair in FProgressItems do
      begin
        if Pair.Value.Status in [psRunning, psPaused] then
          ActiveList.Add(Pair.Value);
      end;
    finally
      FCriticalSection.Release;
    end;
    
    SetLength(Result, ActiveList.Count);
    for var I := 0 to ActiveList.Count - 1 do
      Result[I] := ActiveList[I];
      
  finally
    ActiveList.Free;
  end;
end;

function TProgressTracker.GetOverallProgress: Integer;
var
  TotalProgress: Double;
  Count: Integer;
begin
  Result := 0;
  TotalProgress := 0;
  Count := 0;
  
  FCriticalSection.Acquire;
  try
    for var Pair in FProgressItems do
    begin
      if Pair.Value.Status in [psRunning, psPaused, psCompleted] then
      begin
        TotalProgress := TotalProgress + Pair.Value.Progress;
        Inc(Count);
      end;
    end;
  finally
    FCriticalSection.Release;
  end;
  
  if Count > 0 then
    Result := Round(TotalProgress / Count);
end;

// Métodos de conveniência e utilitários
procedure TProgressTracker.ValidateProgressItem(var ProgressItem: TProgressItem);
begin
  // Validar e corrigir valores se necessário
  if ProgressItem.TotalSteps <= 0 then
    ProgressItem.TotalSteps := 100;
    
  if ProgressItem.MaxRetries < 0 then
    ProgressItem.MaxRetries := 3;
    
  ProgressItem.Progress := Max(0, Min(100, ProgressItem.Progress));
  ProgressItem.SubProgress := Max(0, Min(100, ProgressItem.SubProgress));
end;

function TProgressTracker.ShouldTriggerAlert(const ProgressItem: TProgressItem): Boolean;
var
  ElapsedTime: TDateTime;
begin
  Result := False;
  
  ElapsedTime := Now - ProgressItem.StartTime;
  
  // Alertar se progresso está muito lento
  if (ElapsedTime > (5/24/60)) and (ProgressItem.Progress < 5) then // 5 minutos, menos de 5%
    Result := True;
    
  // Alertar se houve erro
  if ProgressItem.Status = psError then
    Result := True;
    
  // Alertar se muitos retries
  if ProgressItem.RetryCount >= ProgressItem.MaxRetries then
    Result := True;
end;

procedure TProgressTracker.CreateAlert(const ProgressID, AlertType, Message: string; Severity: Integer);
var
  Alert: TProgressAlert;
begin
  Alert.ID := GenerateProgressID;
  Alert.Type_ := AlertType;
  Alert.Message := Message;
  Alert.Timestamp := Now;
  Alert.ProgressID := ProgressID;
  Alert.Severity := Severity;
  Alert.AutoDismiss := Severity < 7;
  Alert.DismissAfter := 30; // 30 segundos para alerts auto-dismiss
  
  FCriticalSection.Acquire;
  try
    FActiveAlerts.Add(Alert);
  finally
    FCriticalSection.Release;
  end;
  
  if Assigned(FOnAlert) then
    FOnAlert(Self, Alert);
end;

// Implementações placeholder para métodos restantes
procedure TProgressTracker.CleanupOldData; begin end;
procedure TProgressTracker.ProcessAlerts; begin end;
procedure TProgressTracker.SendNotification(const ProgressItem: TProgressItem; const NotificationType: string); begin end;
function TProgressTracker.PredictCompletion(const ProgressItem: TProgressItem): TDateTime; begin Result := 0; end;
function TProgressTracker.AnalyzePerformance(const ProgressID: string): TJSONObject; begin Result := TJSONObject.Create; end;
procedure TProgressTracker.RecordBenchmark(const ProgressID: string; const Metric: string; const Value: Double); begin end;
function TProgressTracker.GetAveragePerformance(const MetricName: string): Double; begin Result := 0; end;
procedure TProgressTracker.DismissAlert(const AlertID: string); begin end;
procedure TProgressTracker.RefreshAllVisualizations; begin end;
function TProgressTracker.GetMemoryUsage: Int64; begin Result := 0; end;
function TProgressTracker.GetCPUUsage: Double; begin Result := 0; end;

// Funções globais de conveniência
function StartProgress(const Name, Description: string; TotalSteps: Integer): string;
begin
  if Assigned(GlobalProgressTracker) then
    Result := GlobalProgressTracker.StartProgress(Name, Description, TotalSteps)
  else
    Result := '';
end;

procedure UpdateProgress(const ProgressID: string; Progress: Integer; const CurrentStep: string);
begin
  if Assigned(GlobalProgressTracker) then
    GlobalProgressTracker.UpdateProgress(ProgressID, Progress, CurrentStep);
end;

procedure CompleteProgress(const ProgressID: string; Success: Boolean);
begin
  if Assigned(GlobalProgressTracker) then
    GlobalProgressTracker.CompleteProgress(ProgressID, Success);
end;

procedure SetProgressStage(const ProgressID: string; Stage: TProgressStage);
begin
  if Assigned(GlobalProgressTracker) then
    GlobalProgressTracker.SetProgressStage(ProgressID, Stage);
end;

initialization
  GlobalProgressTracker := TProgressTracker.Create;

finalization
  GlobalProgressTracker.Free;

end.