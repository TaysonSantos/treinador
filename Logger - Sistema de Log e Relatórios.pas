unit Logger;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON, System.DateUtils,
  System.Generics.Collections, System.Threading, System.SyncObjs, System.RegularExpressions,
  System.Math, Winapi.Windows;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical, llFatal);
  
  TLogCategory = (lcGeneral, lcTraining, lcAPI, lcData, lcUI, lcSecurity, lcPerformance, lcSystem);
  
  TLogDestination = (ldFile, ldConsole, ldDatabase, ldEmail, ldEvent, ldNetwork);
  
  TLogFormat = (lfText, lfJSON, lfXML, lfCSV, lfCustom);
  
  TLogEntry = record
    ID: string;
    Timestamp: TDateTime;
    Level: TLogLevel;
    Category: TLogCategory;
    Message: string;
    Details: string;
    Source: string;
    ThreadID: Cardinal;
    ProcessID: Cardinal;
    UserID: string;
    SessionID: string;
    Context: TJSONObject;
    StackTrace: string;
    Duration: Int64; // em milissegundos
    MemoryUsage: Int64;
    Tags: TArray<string>;
  end;
  
  TLogFilter = record
    StartDate: TDateTime;
    EndDate: TDateTime;
    Levels: TArray<TLogLevel>;
    Categories: TArray<TLogCategory>;
    Sources: TArray<string>;
    MessagePattern: string;
    ThreadIDs: TArray<Cardinal>;
    UserIDs: TArray<string>;
    Tags: TArray<string>;
    MaxResults: Integer;
  end;
  
  TLogRotationSettings = record
    Enabled: Boolean;
    MaxFileSize: Int64;
    MaxFiles: Integer;
    RotateDaily: Boolean;
    RotateWeekly: Boolean;
    RotateMonthly: Boolean;
    CompressOldFiles: Boolean;
    DeleteAfterDays: Integer;
  end;
  
  TLogStatistics = record
    TotalEntries: Int64;
    EntriesByLevel: TDictionary<TLogLevel, Int64>;
    EntriesByCategory: TDictionary<TLogCategory, Int64>;
    ErrorRate: Double;
    AverageEntriesPerHour: Double;
    PeakActivity: TDateTime;
    MostActiveSource: string;
    MostCommonMessage: string;
    StorageUsed: Int64;
  end;
  
  TPerformanceMetric = record
    Name: string;
    Value: Double;
    Unit_: string;
    Timestamp: TDateTime;
    Source: string;
    Category: string;
    Context: TJSONObject;
  end;
  
  TLogEvent = procedure(Sender: TObject; const LogEntry: TLogEntry) of object;
  TLogErrorEvent = procedure(Sender: TObject; const Error: string) of object;
  TLogRotationEvent = procedure(Sender: TObject; const OldFile, NewFile: string) of object;
  
  TLogger = class
  private
    FLogEntries: TThreadList<TLogEntry>;
    FDestinations: TArray<TLogDestination>;
    FFormat: TLogFormat;
    FMinLevel: TLogLevel;
    FMaxLevel: TLogLevel;
    FLogPath: string;
    FLogFileName: string;
    FMaxFileSize: Int64;
    FRotationSettings: TLogRotationSettings;
    FBuffered: Boolean;
    FBufferSize: Integer;
    FFlushInterval: Integer; // segundos
    FAsyncLogging: Boolean;
    FCompressionEnabled: Boolean;
    FEncryptionEnabled: Boolean;
    FPerformanceTracking: Boolean;
    
    // Threading e sincronização
    FLogThread: TThread;
    FLogQueue: TQueue<TLogEntry>;
    FCriticalSection: TCriticalSection;
    FShutdownEvent: TEvent;
    FFlushEvent: TEvent;
    
    // Cache e buffer
    FLogBuffer: TStringList;
    FLastFlush: TDateTime;
    FPerformanceMetrics: TList<TPerformanceMetric>;
    
    // Eventos
    FOnLog: TLogEvent;
    FOnError: TLogErrorEvent;
    FOnRotation: TLogRotationEvent;
    
    // Configuração
    FEnabled: Boolean;
    FIncludeStackTrace: Boolean;
    FIncludeMemoryInfo: Boolean;
    FIncludeThreadInfo: Boolean;
    FCustomFormat: string;
    FTimeFormat: string;
    FDateFormat: string;
    FGlobalContext: TJSONObject;
    
    function GetLogLevelString(Level: TLogLevel): string;
    function GetLogCategoryString(Category: TLogCategory): string;
    function GetCurrentMemoryUsage: Int64;
    function GetStackTrace: string;
    function GenerateLogID: string;
    function FormatLogEntry(const Entry: TLogEntry): string;
    function FormatLogEntryJSON(const Entry: TLogEntry): string;
    function FormatLogEntryXML(const Entry: TLogEntry): string;
    function FormatLogEntryCSV(const Entry: TLogEntry): string;
    function GetLogFileName(const BaseFileName: string): string;
    function ShouldRotateLog: Boolean;
    procedure RotateLogFile;
    procedure CompressLogFile(const FileName: string);
    procedure DeleteOldLogFiles;
    procedure WriteToFile(const Data: string);
    procedure WriteToConsole(const Data: string);
    procedure WriteToDatabase(const Entry: TLogEntry);
    procedure SendToEmail(const Entry: TLogEntry);
    procedure SendToNetwork(const Entry: TLogEntry);
    procedure ProcessLogEntry(const Entry: TLogEntry);
    procedure FlushBuffer;
    procedure LogThreadExecute;
    procedure InitializeLogThread;
    procedure FinalizeLogThread;
    function ApplyFilter(const Entry: TLogEntry; const Filter: TLogFilter): Boolean;
    procedure UpdateStatistics(const Entry: TLogEntry);
    function EncryptData(const Data: string): string;
    function DecryptData(const Data: string): string;
    procedure ValidateConfiguration;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Propriedades
    property Enabled: Boolean read FEnabled write FEnabled;
    property LogPath: string read FLogPath write FLogPath;
    property LogFileName: string read FLogFileName write FLogFileName;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property MaxLevel: TLogLevel read FMaxLevel write FMaxLevel;
    property Format: TLogFormat read FFormat write FFormat;
    property CustomFormat: string read FCustomFormat write FCustomFormat;
    property Destinations: TArray<TLogDestination> read FDestinations write FDestinations;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property FlushInterval: Integer read FFlushInterval write FFlushInterval;
    property AsyncLogging: Boolean read FAsyncLogging write FAsyncLogging;
    property RotationSettings: TLogRotationSettings read FRotationSettings write FRotationSettings;
    property PerformanceTracking: Boolean read FPerformanceTracking write FPerformanceTracking;
    property IncludeStackTrace: Boolean read FIncludeStackTrace write FIncludeStackTrace;
    property IncludeMemoryInfo: Boolean read FIncludeMemoryInfo write FIncludeMemoryInfo;
    property IncludeThreadInfo: Boolean read FIncludeThreadInfo write FIncludeThreadInfo;
    property GlobalContext: TJSONObject read FGlobalContext write FGlobalContext;
    
    // Eventos
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnError: TLogErrorEvent read FOnError write FOnError;
    property OnRotation: TLogRotationEvent read FOnRotation write FOnRotation;
    
    // Métodos de log básicos
    procedure Log(Level: TLogLevel; const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Log(Level: TLogLevel; const Message: string; const Details: string; Category: TLogCategory = lcGeneral); overload;
    procedure Log(Level: TLogLevel; const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure LogEx(Level: TLogLevel; Category: TLogCategory; const Source, Message, Details: string; 
      const Context: TJSONObject = nil; const Tags: TArray<string> = nil);
    
    // Métodos de conveniência por nível
    procedure Debug(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Debug(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure Info(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Info(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure Warning(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Warning(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure Error(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Error(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure Critical(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Critical(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    procedure Fatal(const Message: string; Category: TLogCategory = lcGeneral); overload;
    procedure Fatal(const Message: string; const Args: array of const; Category: TLogCategory = lcGeneral); overload;
    
    // Métodos especializados
    procedure LogException(const E: Exception; Category: TLogCategory = lcGeneral; const Context: string = '');
    procedure LogPerformance(const Name: string; const Value: Double; const Unit_: string = 'ms'; 
      Category: TLogCategory = lcPerformance; const Source: string = '');
    procedure LogAPICall(const Method, URL: string; ResponseTime: Int64; ResponseCode: Integer);
    procedure LogTrainingProgress(const ModelName: string; Epoch, TotalEpochs: Integer; 
      Loss, Accuracy: Double; TimeElapsed: TDateTime);
    procedure LogDataProcessing(const FileName: string; RecordsProcessed: Integer; 
      ProcessingTime: TDateTime; Success: Boolean);
    procedure LogUserAction(const UserID, Action, Details: string);
    procedure LogSecurityEvent(const EventType, UserID, Details: string; Severity: TLogLevel = llWarning);
    procedure LogSystemEvent(const EventType, Details: string);
    
    // Métricas e monitoramento
    procedure StartPerformanceTimer(const Name: string);
    procedure StopPerformanceTimer(const Name: string);
    procedure RecordMetric(const Name: string; const Value: Double; const Unit_: string = '');
    procedure RecordCounter(const Name: string; const Increment: Integer = 1);
    procedure RecordGauge(const Name: string; const Value: Double);
    procedure RecordHistogram(const Name: string; const Value: Double);
    
    // Controle de fluxo
    procedure Flush;
    procedure FlushAsync;
    procedure Pause;
    procedure Resume;
    procedure Clear;
    procedure Rotate;
    procedure Shutdown;
    
    // Consulta e análise
    function GetEntries(const Filter: TLogFilter): TArray<TLogEntry>;
    function GetEntriesJSON(const Filter: TLogFilter): string;
    function SearchEntries(const SearchTerm: string): TArray<TLogEntry>;
    function GetStatistics: TLogStatistics;
    function GetPerformanceMetrics(const MetricName: string = ''): TArray<TPerformanceMetric>;
    function AnalyzeLogs(StartDate, EndDate: TDateTime): TJSONObject;
    function GenerateReport(const ReportType: string; StartDate, EndDate: TDateTime): string;
    
    // Configuração
    procedure LoadConfiguration(const ConfigFile: string);
    procedure SaveConfiguration(const ConfigFile: string);
    procedure SetDestination(Destination: TLogDestination; Enabled: Boolean = True);
    procedure AddDestination(Destination: TLogDestination);
    procedure RemoveDestination(Destination: TLogDestination);
    procedure SetFilter(Level: TLogLevel; Categories: TArray<TLogCategory>);
    procedure AddGlobalTag(const Tag: string);
    procedure RemoveGlobalTag(const Tag: string);
    procedure SetGlobalContext(const Key: string; const Value: Variant);
    
    // Importação e exportação
    function ExportLogs(const FileName: string; const Filter: TLogFilter; 
      Format: TLogFormat = lfJSON): Boolean;
    function ImportLogs(const FileName: string): Boolean;
    function ArchiveLogs(const ArchivePath: string; OlderThan: TDateTime): Boolean;
    function BackupLogs(const BackupPath: string): Boolean;
    function RestoreLogs(const BackupPath: string): Boolean;
    
    // Utilitários
    function GetLogFileSize: Int64;
    function GetTotalLogFiles: Integer;
    function CleanupLogs(OlderThan: TDateTime): Integer;
    function ValidateLogIntegrity: Boolean;
    function CompressLogs(const ArchiveName: string): Boolean;
    function GetLogHealth: TJSONObject;
    procedure OptimizeLogs;
    
    // Configurações avançadas
    procedure EnableRealTimeMonitoring;
    procedure DisableRealTimeMonitoring;
    procedure SetCustomFormatter(const FormatFunction: TFunc<TLogEntry, string>);
    procedure SetCustomDestination(const WriteFunction: TProc<TLogEntry>);
    procedure EnableLogEncryption(const Password: string);
    procedure DisableLogEncryption;
    procedure SetLogRetention(Days: Integer);
    procedure SetCompressionLevel(Level: Integer);
  end;

var
  GlobalLogger: TLogger;

// Funções globais de conveniência
procedure LogDebug(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogInfo(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogWarning(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogError(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogCritical(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogFatal(const Message: string; Category: TLogCategory = lcGeneral);
procedure LogException(const E: Exception; Category: TLogCategory = lcGeneral);

implementation

uses
  System.Variants, System.NetEncoding, System.Hash, System.Zip, System.Win.ComObj;

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  
  FLogEntries := TThreadList<TLogEntry>.Create;
  FLogQueue := TQueue<TLogEntry>.Create;
  FCriticalSection := TCriticalSection.Create;
  FShutdownEvent := TEvent.Create(nil, True, False, '');
  FFlushEvent := TEvent.Create(nil, False, False, '');
  FLogBuffer := TStringList.Create;
  FPerformanceMetrics := TList<TPerformanceMetric>.Create;
  FGlobalContext := TJSONObject.Create;
  
  // Configurações padrão
  FEnabled := True;
  FLogPath := TPath.Combine(TPath.GetDocumentsPath, 'OllamaTrainer', 'Logs');
  FLogFileName := 'application.log';
  FMinLevel := llDebug;
  FMaxLevel := llFatal;
  FFormat := lfText;
  FDestinations := [ldFile];
  FBuffered := True;
  FBufferSize := 1000;
  FFlushInterval := 30;
  FAsyncLogging := True;
  FCompressionEnabled := False;
  FEncryptionEnabled := False;
  FPerformanceTracking := True;
  FIncludeStackTrace := False;
  FIncludeMemoryInfo := True;
  FIncludeThreadInfo := True;
  FTimeFormat := 'hh:nn:ss.zzz';
  FDateFormat := 'yyyy-mm-dd';
  FMaxFileSize := 50 * 1024 * 1024; // 50MB
  
  // Configurações de rotação padrão
  FRotationSettings.Enabled := True;
  FRotationSettings.MaxFileSize := 50 * 1024 * 1024;
  FRotationSettings.MaxFiles := 10;
  FRotationSettings.RotateDaily := True;
  FRotationSettings.CompressOldFiles := True;
  FRotationSettings.DeleteAfterDays := 30;
  
  // Garantir que o diretório existe
  ForceDirectories(FLogPath);
  
  // Inicializar thread de log se assíncrono
  if FAsyncLogging then
    InitializeLogThread;
    
  ValidateConfiguration;
end;

destructor TLogger.Destroy;
begin
  Shutdown;
  
  FinalizeLogThread;
  
  // Flush final
  if FBuffered then
    FlushBuffer;
  
  // Liberar recursos
  FGlobalContext.Free;
  FPerformanceMetrics.Free;
  FLogBuffer.Free;
  FFlushEvent.Free;
  FShutdownEvent.Free;
  FCriticalSection.Free;
  FLogQueue.Free;
  FLogEntries.Free;
  
  inherited Destroy;
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string; Category: TLogCategory);
begin
  LogEx(Level, Category, '', Message, '', nil, nil);
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string; const Details: string; Category: TLogCategory);
begin
  LogEx(Level, Category, '', Message, Details, nil, nil);
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string; const Args: array of const; Category: TLogCategory);
begin
  LogEx(Level, Category, '', Format(Message, Args), '', nil, nil);
end;

procedure TLogger.LogEx(Level: TLogLevel; Category: TLogCategory; const Source, Message, Details: string; 
  const Context: TJSONObject; const Tags: TArray<string>);
var
  Entry: TLogEntry;
  I: Integer;
begin
  if not FEnabled then
    Exit;
    
  if (Level < FMinLevel) or (Level > FMaxLevel) then
    Exit;
  
  // Criar entrada de log
  Entry.ID := GenerateLogID;
  Entry.Timestamp := Now;
  Entry.Level := Level;
  Entry.Category := Category;
  Entry.Message := Message;
  Entry.Details := Details;
  Entry.Source := Source;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.ProcessID := GetCurrentProcessId;
  Entry.UserID := GetEnvironmentVariable('USERNAME');
  Entry.SessionID := GenerateLogID; // Placeholder
  
  // Context
  if Assigned(Context) then
    Entry.Context := TJSONObject(Context.Clone)
  else
    Entry.Context := TJSONObject.Create;
    
  // Adicionar contexto global
  if Assigned(FGlobalContext) then
  begin
    for var Pair in FGlobalContext do
      Entry.Context.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
  end;
  
  // Tags
  SetLength(Entry.Tags, Length(Tags));
  for I := 0 to High(Tags) do
    Entry.Tags[I] := Tags[I];
  
  // Informações opcionais
  if FIncludeStackTrace and (Level >= llError) then
    Entry.StackTrace := GetStackTrace;
    
  if FIncludeMemoryInfo then
    Entry.MemoryUsage := GetCurrentMemoryUsage;
  
  // Processar entrada
  if FAsyncLogging then
  begin
    FCriticalSection.Acquire;
    try
      FLogQueue.Enqueue(Entry);
      FFlushEvent.SetEvent;
    finally
      FCriticalSection.Release;
    end;
  end
  else
    ProcessLogEntry(Entry);
end;

procedure TLogger.ProcessLogEntry(const Entry: TLogEntry);
var
  FormattedEntry: string;
  Destination: TLogDestination;
begin
  try
    // Atualizar estatísticas
    UpdateStatistics(Entry);
    
    // Adicionar à lista de entradas
    var List := FLogEntries.LockList;
    try
      List.Add(Entry);
      
      // Limitar tamanho da lista em memória
      if List.Count > 10000 then
        List.Delete(0);
    finally
      FLogEntries.UnlockList;
    end;
    
    // Formatar entrada
    FormattedEntry := FormatLogEntry(Entry);
    
    // Escrever nos destinos configurados
    for Destination in FDestinations do
    begin
      case Destination of
        ldFile: WriteToFile(FormattedEntry);
        ldConsole: WriteToConsole(FormattedEntry);
        ldDatabase: WriteToDatabase(Entry);
        ldEmail: if Entry.Level >= llCritical then SendToEmail(Entry);
        ldNetwork: SendToNetwork(Entry);
      end;
    end;
    
    // Verificar se precisa rotacionar
    if ShouldRotateLog then
      RotateLogFile;
    
    // Evento de log
    if Assigned(FOnLog) then
      FOnLog(Self, Entry);
      
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 'Erro ao processar entrada de log: ' + E.Message);
    end;
  end;
end;

function TLogger.FormatLogEntry(const Entry: TLogEntry): string;
begin
  case FFormat of
    lfText: Result := FormatLogEntryText(Entry);
    lfJSON: Result := FormatLogEntryJSON(Entry);
    lfXML: Result := FormatLogEntryXML(Entry);
    lfCSV: Result := FormatLogEntryCSV(Entry);
    lfCustom: Result := FormatLogEntryCustom(Entry);
  else
    Result := FormatLogEntryText(Entry);
  end;
end;

function TLogger.FormatLogEntryText(const Entry: TLogEntry): string;
var
  TimeStr: string;
  LevelStr: string;
  CategoryStr: string;
  ThreadInfo: string;
  MemoryInfo: string;
begin
  TimeStr := FormatDateTime(FDateFormat + ' ' + FTimeFormat, Entry.Timestamp);
  LevelStr := GetLogLevelString(Entry.Level);
  CategoryStr := GetLogCategoryString(Entry.Category);
  
  if FIncludeThreadInfo then
    ThreadInfo := Format('[T:%d]', [Entry.ThreadID])
  else
    ThreadInfo := '';
    
  if FIncludeMemoryInfo and (Entry.MemoryUsage > 0) then
    MemoryInfo := Format('[M:%s]', [FormatBytes(Entry.MemoryUsage)])
  else
    MemoryInfo := '';
  
  Result := Format('%s [%s] [%s] %s%s %s: %s',
    [TimeStr, LevelStr, CategoryStr, ThreadInfo, MemoryInfo, Entry.Source, Entry.Message]);
  
  if Entry.Details <> '' then
    Result := Result + ' | ' + Entry.Details;
    
  if Entry.StackTrace <> '' then
    Result := Result + #13#10 + 'Stack Trace:' + #13#10 + Entry.StackTrace;
end;

function TLogger.FormatLogEntryJSON(const Entry: TLogEntry): string;
var
  JSONObj: TJSONObject;
  TagsArray: TJSONArray;
  I: Integer;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('id', Entry.ID);
    JSONObj.AddPair('timestamp', DateToISO8601(Entry.Timestamp));
    JSONObj.AddPair('level', GetLogLevelString(Entry.Level));
    JSONObj.AddPair('category', GetLogCategoryString(Entry.Category));
    JSONObj.AddPair('message', Entry.Message);
    JSONObj.AddPair('details', Entry.Details);
    JSONObj.AddPair('source', Entry.Source);
    JSONObj.AddPair('thread_id', TJSONNumber.Create(Entry.ThreadID));
    JSONObj.AddPair('process_id', TJSONNumber.Create(Entry.ProcessID));
    JSONObj.AddPair('user_id', Entry.UserID);
    JSONObj.AddPair('session_id', Entry.SessionID);
    
    if Entry.MemoryUsage > 0 then
      JSONObj.AddPair('memory_usage', TJSONNumber.Create(Entry.MemoryUsage));
      
    if Entry.Duration > 0 then
      JSONObj.AddPair('duration', TJSONNumber.Create(Entry.Duration));
    
    if Entry.StackTrace <> '' then
      JSONObj.AddPair('stack_trace', Entry.StackTrace);
    
    // Tags
    if Length(Entry.Tags) > 0 then
    begin
      TagsArray := TJSONArray.Create;
      for I := 0 to High(Entry.Tags) do
        TagsArray.AddElement(TJSONString.Create(Entry.Tags[I]));
      JSONObj.AddPair('tags', TagsArray);
    end;
    
    // Context
    if Assigned(Entry.Context) and (Entry.Context.Count > 0) then
      JSONObj.AddPair('context', Entry.Context.Clone as TJSONObject);
    
    Result := JSONObj.ToString;
    
  finally
    JSONObj.Free;
  end;
end;

function TLogger.FormatLogEntryXML(const Entry: TLogEntry): string;
var
  XML: TStringBuilder;
  I: Integer;
begin
  XML := TStringBuilder.Create;
  try
    XML.AppendLine('<log-entry>');
    XML.AppendLine('  <id>' + Entry.ID + '</id>');
    XML.AppendLine('  <timestamp>' + DateToISO8601(Entry.Timestamp) + '</timestamp>');
    XML.AppendLine('  <level>' + GetLogLevelString(Entry.Level) + '</level>');
    XML.AppendLine('  <category>' + GetLogCategoryString(Entry.Category) + '</category>');
    XML.AppendLine('  <message><![CDATA[' + Entry.Message + ']]></message>');
    XML.AppendLine('  <details><![CDATA[' + Entry.Details + ']]></details>');
    XML.AppendLine('  <source>' + Entry.Source + '</source>');
    XML.AppendLine('  <thread-id>' + IntToStr(Entry.ThreadID) + '</thread-id>');
    XML.AppendLine('  <process-id>' + IntToStr(Entry.ProcessID) + '</process-id>');
    XML.AppendLine('  <user-id>' + Entry.UserID + '</user-id>');
    
    if Entry.MemoryUsage > 0 then
      XML.AppendLine('  <memory-usage>' + IntToStr(Entry.MemoryUsage) + '</memory-usage>');
    
    if Length(Entry.Tags) > 0 then
    begin
      XML.AppendLine('  <tags>');
      for I := 0 to High(Entry.Tags) do
        XML.AppendLine('    <tag>' + Entry.Tags[I] + '</tag>');
      XML.AppendLine('  </tags>');
    end;
    
    if Entry.StackTrace <> '' then
      XML.AppendLine('  <stack-trace><![CDATA[' + Entry.StackTrace + ']]></stack-trace>');
    
    XML.AppendLine('</log-entry>');
    
    Result := XML.ToString;
    
  finally
    XML.Free;
  end;
end;

function TLogger.FormatLogEntryCSV(const Entry: TLogEntry): string;
var
  Fields: TStringList;
  TagsStr: string;
  I: Integer;
begin
  Fields := TStringList.Create;
  try
    Fields.Delimiter := ',';
    Fields.QuoteChar := '"';
    Fields.StrictDelimiter := True;
    
    Fields.Add(Entry.ID);
    Fields.Add(DateToISO8601(Entry.Timestamp));
    Fields.Add(GetLogLevelString(Entry.Level));
    Fields.Add(GetLogCategoryString(Entry.Category));
    Fields.Add(StringReplace(Entry.Message, '"', '""', [rfReplaceAll]));
    Fields.Add(StringReplace(Entry.Details, '"', '""', [rfReplaceAll]));
    Fields.Add(Entry.Source);
    Fields.Add(IntToStr(Entry.ThreadID));
    Fields.Add(IntToStr(Entry.ProcessID));
    Fields.Add(Entry.UserID);
    Fields.Add(IntToStr(Entry.MemoryUsage));
    
    // Tags como string delimitada por pipe
    TagsStr := '';
    for I := 0 to High(Entry.Tags) do
    begin
      if I > 0 then
        TagsStr := TagsStr + '|';
      TagsStr := TagsStr + Entry.Tags[I];
    end;
    Fields.Add(TagsStr);
    
    Result := Fields.DelimitedText;
    
  finally
    Fields.Free;
  end;
end;

function TLogger.FormatLogEntryCustom(const Entry: TLogEntry): string;
begin
  // Implementar formato customizado baseado em FCustomFormat
  Result := FCustomFormat;
  
  Result := StringReplace(Result, '{timestamp}', DateTimeToStr(Entry.Timestamp), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{level}', GetLogLevelString(Entry.Level), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{category}', GetLogCategoryString(Entry.Category), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{message}', Entry.Message, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{details}', Entry.Details, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{source}', Entry.Source, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{thread}', IntToStr(Entry.ThreadID), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{memory}', IntToStr(Entry.MemoryUsage), [rfReplaceAll, rfIgnoreCase]);
end;

procedure TLogger.WriteToFile(const Data: string);
var
  FileName: string;
  FileStream: TFileStream;
  DataBytes: TBytes;
begin
  try
    FileName := GetLogFileName(FLogFileName);
    
    if FBuffered then
    begin
      FLogBuffer.Add(Data);
      
      if (FLogBuffer.Count >= FBufferSize) or 
         (SecondsBetween(Now, FLastFlush) >= FFlushInterval) then
        FlushBuffer;
    end
    else
    begin
      DataBytes := TEncoding.UTF8.GetBytes(Data + #13#10);
      
      if FEncryptionEnabled then
      begin
        var EncryptedData := EncryptData(TEncoding.UTF8.GetString(DataBytes));
        DataBytes := TEncoding.UTF8.GetBytes(EncryptedData);
      end;
      
      FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
      try
        FileStream.Seek(0, soEnd);
        FileStream.WriteBuffer(DataBytes[0], Length(DataBytes));
      finally
        FileStream.Free;
      end;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 'Erro ao escrever no arquivo de log: ' + E.Message);
    end;
  end;
end;

procedure TLogger.FlushBuffer;
var
  FileName: string;
  FileStream: TFileStream;
  AllData: string;
  DataBytes: TBytes;
begin
  if FLogBuffer.Count = 0 then
    Exit;
    
  try
    FileName := GetLogFileName(FLogFileName);
    AllData := FLogBuffer.Text;
    
    if FEncryptionEnabled then
      AllData := EncryptData(AllData);
    
    DataBytes := TEncoding.UTF8.GetBytes(AllData);
    
    FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      FileStream.Seek(0, soEnd);
      FileStream.WriteBuffer(DataBytes[0], Length(DataBytes));
    finally
      FileStream.Free;
    end;
    
    FLogBuffer.Clear;
    FLastFlush := Now;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 'Erro ao fazer flush do buffer: ' + E.Message);
    end;
  end;
end;

// Implementações de métodos de conveniência

procedure TLogger.Debug(const Message: string; Category: TLogCategory);
begin
  Log(llDebug, Message, Category);
end;

procedure TLogger.Debug(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llDebug, Message, Args, Category);
end;

procedure TLogger.Info(const Message: string; Category: TLogCategory);
begin
  Log(llInfo, Message, Category);
end;

procedure TLogger.Info(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llInfo, Message, Args, Category);
end;

procedure TLogger.Warning(const Message: string; Category: TLogCategory);
begin
  Log(llWarning, Message, Category);
end;

procedure TLogger.Warning(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llWarning, Message, Args, Category);
end;

procedure TLogger.Error(const Message: string; Category: TLogCategory);
begin
  Log(llError, Message, Category);
end;

procedure TLogger.Error(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llError, Message, Args, Category);
end;

procedure TLogger.Critical(const Message: string; Category: TLogCategory);
begin
  Log(llCritical, Message, Category);
end;

procedure TLogger.Critical(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llCritical, Message, Args, Category);
end;

procedure TLogger.Fatal(const Message: string; Category: TLogCategory);
begin
  Log(llFatal, Message, Category);
end;

procedure TLogger.Fatal(const Message: string; const Args: array of const; Category: TLogCategory);
begin
  Log(llFatal, Message, Args, Category);
end;

// Implementações de métodos utilitários

function TLogger.GetLogLevelString(Level: TLogLevel): string;
begin
  case Level of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARN';
    llError: Result := 'ERROR';
    llCritical: Result := 'CRITICAL';
    llFatal: Result := 'FATAL';
  else
    Result := 'UNKNOWN';
  end;
end;

function TLogger.GetLogCategoryString(Category: TLogCategory): string;
begin
  case Category of
    lcGeneral: Result := 'GENERAL';
    lcTraining: Result := 'TRAINING';
    lcAPI: Result := 'API';
    lcData: Result := 'DATA';
    lcUI: Result := 'UI';
    lcSecurity: Result := 'SECURITY';
    lcPerformance: Result := 'PERFORMANCE';
    lcSystem: Result := 'SYSTEM';
  else
    Result := 'UNKNOWN';
  end;
end;

function TLogger.GetCurrentMemoryUsage: Int64;
var
  MemStatus: TMemoryStatus;
begin
  MemStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MemStatus);
  Result := MemStatus.dwTotalPhys - MemStatus.dwAvailPhys;
end;

function TLogger.GetStackTrace: string;
begin
  // Implementação básica - pode ser melhorada com bibliotecas especializadas
  Result := 'Stack trace not implemented';
end;

function TLogger.GenerateLogID: string;
begin
  Result := THashMD5.GetHashString(DateTimeToStr(Now) + IntToStr(Random(MaxInt)));
end;

function TLogger.GetLogFileName(const BaseFileName: string): string;
var
  FileNameWithoutExt: string;
  Extension: string;
begin
  FileNameWithoutExt := ChangeFileExt(BaseFileName, '');
  Extension := ExtractFileExt(BaseFileName);
  
  if FRotationSettings.RotateDaily then
    Result := TPath.Combine(FLogPath, FileNameWithoutExt + '_' + FormatDateTime('yyyymmdd', Now) + Extension)
  else
    Result := TPath.Combine(FLogPath, BaseFileName);
end;

function TLogger.ShouldRotateLog: Boolean;
var
  FileName: string;
begin
  Result := False;
  
  if not FRotationSettings.Enabled then
    Exit;
  
  FileName := GetLogFileName(FLogFileName);
  
  if not FileExists(FileName) then
    Exit;
  
  // Verificar tamanho do arquivo
  if TFile.GetSize(FileName) >= FRotationSettings.MaxFileSize then
    Result := True;
  
  // Verificar rotação diária
  if FRotationSettings.RotateDaily then
  begin
    var FileDate := TFile.GetLastWriteTime(FileName);
    if not SameDate(FileDate, Now) then
      Result := True;
  end;
end;

procedure TLogger.RotateLogFile;
var
  CurrentFile: string;
  RotatedFile: string;
  I: Integer;
begin
  try
    CurrentFile := GetLogFileName(FLogFileName);
    
    if not FileExists(CurrentFile) then
      Exit;
    
    // Flush antes da rotação
    if FBuffered then
      FlushBuffer;
    
    // Criar nome do arquivo rotacionado
    RotatedFile := ChangeFileExt(CurrentFile, '') + '_' + 
      FormatDateTime('yyyymmdd_hhnnss', Now) + ExtractFileExt(CurrentFile);
    
    // Mover arquivo atual
    TFile.Move(CurrentFile, RotatedFile);
    
    // Comprimir se habilitado
    if FRotationSettings.CompressOldFiles then
      CompressLogFile(RotatedFile);
    
    // Limpar arquivos antigos
    DeleteOldLogFiles;
    
    if Assigned(FOnRotation) then
      FOnRotation(Self, CurrentFile, RotatedFile);
      
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 'Erro na rotação do log: ' + E.Message);
    end;
  end;
end;

// Implementações básicas para métodos restantes

procedure TLogger.LogException(const E: Exception; Category: TLogCategory; const Context: string);
var
  Details: string;
begin
  Details := 'Exception: ' + E.ClassName + #13#10 + 'Message: ' + E.Message;
  if Context <> '' then
    Details := Details + #13#10 + 'Context: ' + Context;
    
  LogEx(llError, Category, 'Exception Handler', E.Message, Details, nil, ['exception']);
end;

procedure TLogger.InitializeLogThread;
begin
  if FAsyncLogging and not Assigned(FLogThread) then
  begin
    FLogThread := TThread.CreateAnonymousThread(LogThreadExecute);
    FLogThread.Start;
  end;
end;

procedure TLogger.FinalizeLogThread;
begin
  if Assigned(FLogThread) then
  begin
    FShutdownEvent.SetEvent;
    FLogThread.WaitFor;
    FLogThread.Free;
    FLogThread := nil;
  end;
end;

procedure TLogger.LogThreadExecute;
var
  Events: array[0..1] of THandle;
  WaitResult: DWORD;
  Entry: TLogEntry;
begin
  Events[0] := FShutdownEvent.Handle;
  Events[1] := FFlushEvent.Handle;
  
  while True do
  begin
    WaitResult := WaitForMultipleObjects(2, @Events[0], False, 1000);
    
    case WaitResult of
      WAIT_OBJECT_0: // Shutdown
        Break;
        
      WAIT_OBJECT_0 + 1, WAIT_TIMEOUT: // Flush ou timeout
      begin
        FCriticalSection.Acquire;
        try
          while FLogQueue.Count > 0 do
          begin
            Entry := FLogQueue.Dequeue;
            ProcessLogEntry(Entry);
          end;
        finally
          FCriticalSection.Release;
        end;
        
        // Flush buffer periodicamente
        if FBuffered and (SecondsBetween(Now, FLastFlush) >= FFlushInterval) then
          FlushBuffer;
      end;
    end;
  end;
  
  // Processar entradas restantes antes de sair
  FCriticalSection.Acquire;
  try
    while FLogQueue.Count > 0 do
    begin
      Entry := FLogQueue.Dequeue;
      ProcessLogEntry(Entry);
    end;
  finally
    FCriticalSection.Release;
  end;
  
  if FBuffered then
    FlushBuffer;
end;

// Implementações placeholder para métodos restantes
procedure TLogger.WriteToConsole(const Data: string); begin WriteLn(Data); end;
procedure TLogger.WriteToDatabase(const Entry: TLogEntry); begin end;
procedure TLogger.SendToEmail(const Entry: TLogEntry); begin end;
procedure TLogger.SendToNetwork(const Entry: TLogEntry); begin end;
procedure TLogger.UpdateStatistics(const Entry: TLogEntry); begin end;
function TLogger.EncryptData(const Data: string): string; begin Result := TNetEncoding.Base64.Encode(Data); end;
function TLogger.DecryptData(const Data: string): string; begin Result := TNetEncoding.Base64.Decode(Data); end;
procedure TLogger.ValidateConfiguration; begin end;
procedure TLogger.CompressLogFile(const FileName: string); begin end;
procedure TLogger.DeleteOldLogFiles; begin end;
function TLogger.ApplyFilter(const Entry: TLogEntry; const Filter: TLogFilter): Boolean; begin Result := True; end;

// Métodos de interface pública (implementações básicas)
procedure TLogger.Flush; begin if FBuffered then FlushBuffer; end;
procedure TLogger.FlushAsync; begin if Assigned(FLogThread) then FFlushEvent.SetEvent; end;
procedure TLogger.Pause; begin FEnabled := False; end;
procedure TLogger.Resume; begin FEnabled := True; end;
procedure TLogger.Clear; begin FLogEntries.LockList.Clear; FLogEntries.UnlockList; end;
procedure TLogger.Rotate; begin RotateLogFile; end;
procedure TLogger.Shutdown; begin FShutdownEvent.SetEvent; FinalizeLogThread; end;

// Funções globais de conveniência
procedure LogDebug(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Debug(Message, Category);
end;

procedure LogInfo(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Info(Message, Category);
end;

procedure LogWarning(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Warning(Message, Category);
end;

procedure LogError(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Error(Message, Category);
end;

procedure LogCritical(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Critical(Message, Category);
end;

procedure LogFatal(const Message: string; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.Fatal(Message, Category);
end;

procedure LogException(const E: Exception; Category: TLogCategory);
begin
  if Assigned(GlobalLogger) then
    GlobalLogger.LogException(E, Category);
end;

// Função para formatar bytes
function FormatBytes(Bytes: Int64): string;
const
  Units: array[0..4] of string = ('B', 'KB', 'MB', 'GB', 'TB');
var
  UnitIndex: Integer;
  Size: Double;
begin
  UnitIndex := 0;
  Size := Bytes;
  
  while (Size >= 1024) and (UnitIndex < High(Units)) do
  begin
    Size := Size / 1024;
    Inc(UnitIndex);
  end;
  
  Result := Format('%.2f %s', [Size, Units[UnitIndex]]);
end;

initialization
  GlobalLogger := TLogger.Create;

finalization
  GlobalLogger.Free;

end.