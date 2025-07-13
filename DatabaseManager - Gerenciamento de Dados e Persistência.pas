unit DatabaseManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.IOUtils, System.DateUtils, System.Variants, System.Threading,
  Data.DB, Data.SqlExpr, Data.DbxSqlite, FireDAC.Comp.Client, FireDAC.Stan.Def,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs;

type
  TDatabaseType = (dtSQLite, dtMySQL, dtPostgreSQL, dtFirebird, dtMSSQL, dtOracle);
  
  TBackupType = (btFull, btIncremental, btDifferential, btTransaction);
  
  TQueryResultFormat = (qrfDataSet, qrfJSON, qrfXML, qrfCSV, qrfArray);
  
  TConnectionPool = class;
  
  TDatabaseConnection = record
    ID: string;
    DatabaseType: TDatabaseType;
    HostName: string;
    Port: Integer;
    DatabaseName: string;
    Username: string;
    Password: string;
    ConnectionString: string;
    IsActive: Boolean;
    CreatedAt: TDateTime;
    LastUsed: TDateTime;
    UsageCount: Integer;
    MaxIdleTime: Integer; // seconds
    Timeout: Integer; // seconds
    PoolSize: Integer;
    Properties: TJSONObject;
  end;
  
  TTableSchema = record
    TableName: string;
    Columns: TArray<record
      Name: string;
      DataType: string;
      Size: Integer;
      Precision: Integer;
      Scale: Integer;
      Nullable: Boolean;
      DefaultValue: Variant;
      IsPrimaryKey: Boolean;
      IsForeignKey: Boolean;
      ForeignKeyTable: string;
      ForeignKeyColumn: string;
      IsUnique: Boolean;
      IsIndexed: Boolean;
      Comment: string;
    end>;
    Indexes: TArray<record
      Name: string;
      Columns: TArray<string>;
      IsUnique: Boolean;
      IsPrimary: Boolean;
    end>;
    Constraints: TArray<record
      Name: string;
      Type_: string; // CHECK, FOREIGN KEY, UNIQUE, etc.
      Definition: string;
    end>;
    CreatedAt: TDateTime;
    ModifiedAt: TDateTime;
    Version: string;
  end;
  
  TBackupInfo = record
    BackupID: string;
    BackupType: TBackupType;
    FileName: string;
    FilePath: string;
    FileSize: Int64;
    CreatedAt: TDateTime;
    CompletedAt: TDateTime;
    Duration: TDateTime;
    Compressed: Boolean;
    Encrypted: Boolean;
    CheckSum: string;
    Tables: TArray<string>;
    RecordCount: Int64;
    Success: Boolean;
    ErrorMessage: string;
    Metadata: TJSONObject;
  end;
  
  TMigrationScript = record
    Version: string;
    Name: string;
    Description: string;
    UpScript: string;
    DownScript: string;
    CreatedAt: TDateTime;
    AppliedAt: TDateTime;
    IsApplied: Boolean;
    ExecutionTime: TDateTime;
    CheckSum: string;
  end;
  
  TQueryOptimization = record
    QueryHash: string;
    OriginalQuery: string;
    OptimizedQuery: string;
    ExecutionPlan: string;
    OriginalTime: Double;
    OptimizedTime: Double;
    Improvement: Double;
    Suggestions: TArray<string>;
    CreatedAt: TDateTime;
  end;
  
  TDatabaseEvent = procedure(Sender: TObject; const EventType: string; 
    const Data: TJSONObject) of object;
  TBackupProgressEvent = procedure(Sender: TObject; Progress: Integer; 
    const Status: string) of object;
  TQueryExecutedEvent = procedure(Sender: TObject; const SQL: string; 
    ExecutionTime: Double; RecordsAffected: Integer) of object;
    
  TDatabaseManager = class
  private
    FConnections: TDictionary<string, TDatabaseConnection>;
    FConnectionPools: TDictionary<string, TConnectionPool>;
    FSchemas: TDictionary<string, TTableSchema>;
    FBackups: TList<TBackupInfo>;
    FMigrations: TList<TMigrationScript>;
    FQueryOptimizations: TDictionary<string, TQueryOptimization>;
    
    // FireDAC Components
    FFDManager: TFDManager;
    FFDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FFDGUIxWaitCursor: TFDGUIxWaitCursor;
    
    // Configuration
    FDatabasePath: string;
    FMaxConnections: Integer;
    FConnectionTimeout: Integer;
    FQueryTimeout: Integer;
    FAutoBackup: Boolean;
    FBackupInterval: Integer; // hours
    FCompressionLevel: Integer;
    FEncryptionEnabled: Boolean;
    FEncryptionKey: string;
    FQueryLogging: Boolean;
    FPerformanceMonitoring: Boolean;
    FAutoOptimize: Boolean;
    FMainConnection: TFDConnection;
    
    // Threading
    FBackupThread: TThread;
    FMaintenanceThread: TThread;
    FShutdownEvent: TEvent;
    
    // Events
    FOnDatabaseEvent: TDatabaseEvent;
    FOnBackupProgress: TBackupProgressEvent;
    FOnQueryExecuted: TQueryExecutedEvent;
    
    // Cache
    FQueryCache: TDictionary<string, TJSONArray>;
    FCacheEnabled: Boolean;
    FCacheTimeout: Integer; // minutes
    FCacheMaxSize: Integer; // MB
    FCacheHitCount: Int64;
    FCacheMissCount: Int64;
    
    function InitializeDatabase: Boolean;
    function CreateSystemTables: Boolean;
    function LoadConnectionFromConfig(const ConfigFile: string): Boolean;
    function CreateConnection(const ConnectionInfo: TDatabaseConnection): TFDConnection;
    function GetConnectionString(const ConnectionInfo: TDatabaseConnection): string;
    function ValidateConnection(const ConnectionInfo: TDatabaseConnection): Boolean;
    function HashQuery(const SQL: string): string;
    function OptimizeQuery(const SQL: string): string;
    function ExecuteQueryInternal(const SQL: string; const Params: TArray<Variant>;
      Connection: TFDConnection; Format: TQueryResultFormat): Variant;
    function ConvertDataSetToJSON(DataSet: TDataSet): TJSONArray;
    function ConvertDataSetToXML(DataSet: TDataSet): string;
    function ConvertDataSetToCSV(DataSet: TDataSet): string;
    function ConvertDataSetToArray(DataSet: TDataSet): TArray<TArray<Variant>>;
    function CreateBackupFileName(BackupType: TBackupType): string;
    function CompressBackup(const FileName: string): Boolean;
    function EncryptBackup(const FileName: string): Boolean;
    function CalculateCheckSum(const FileName: string): string;
    procedure BackupThreadExecute;
    procedure MaintenanceThreadExecute;
    procedure LogQuery(const SQL: string; ExecutionTime: Double; RecordsAffected: Integer);
    procedure UpdateStatistics(const OperationType: string; const Metadata: TJSONObject);
    procedure CleanupCache;
    procedure OptimizeDatabase;
    procedure AnalyzeTableStatistics;
    procedure RebuildIndexes;
    function GetCacheKey(const SQL: string; const Params: TArray<Variant>): string;
    function IsCacheValid(const CacheKey: string): Boolean;
    procedure InvalidateCache(const Pattern: string = '');
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Properties
    property DatabasePath: string read FDatabasePath write FDatabasePath;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
    property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
    property AutoBackup: Boolean read FAutoBackup write FAutoBackup;
    property BackupInterval: Integer read FBackupInterval write FBackupInterval;
    property EncryptionEnabled: Boolean read FEncryptionEnabled write FEncryptionEnabled;
    property QueryLogging: Boolean read FQueryLogging write FQueryLogging;
    property PerformanceMonitoring: Boolean read FPerformanceMonitoring write FPerformanceMonitoring;
    property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
    property CacheTimeout: Integer read FCacheTimeout write FCacheTimeout;
    
    // Events
    property OnDatabaseEvent: TDatabaseEvent read FOnDatabaseEvent write FOnDatabaseEvent;
    property OnBackupProgress: TBackupProgressEvent read FOnBackupProgress write FOnBackupProgress;
    property OnQueryExecuted: TQueryExecutedEvent read FOnQueryExecuted write FOnQueryExecuted;
    
    // Connection Management
    function AddConnection(const ConnectionInfo: TDatabaseConnection): Boolean;
    function RemoveConnection(const ConnectionID: string): Boolean;
    function GetConnection(const ConnectionID: string = ''): TFDConnection;
    function TestConnection(const ConnectionID: string): Boolean;
    function GetActiveConnections: TArray<string>;
    function CloseConnection(const ConnectionID: string): Boolean;
    function CloseAllConnections: Boolean;
    
    // Database Operations
    function Initialize(const DatabasePath: string = ''): Boolean;
    function CreateDatabase(const DatabaseName: string; const ConnectionInfo: TDatabaseConnection): Boolean;
    function DropDatabase(const DatabaseName: string): Boolean;
    function DatabaseExists(const DatabaseName: string): Boolean;
    function GetDatabaseSize(const DatabaseName: string = ''): Int64;
    function GetDatabaseInfo(const DatabaseName: string = ''): TJSONObject;
    function VacuumDatabase(const DatabaseName: string = ''): Boolean;
    function AnalyzeDatabase(const DatabaseName: string = ''): Boolean;
    
    // Query Execution
    function ExecuteQuery(const SQL: string; Format: TQueryResultFormat = qrfJSON): Variant; overload;
    function ExecuteQuery(const SQL: string; const Params: TArray<Variant>; 
      Format: TQueryResultFormat = qrfJSON): Variant; overload;
    function ExecuteQueryAsync(const SQL: string; const Params: TArray<Variant> = nil): ITask<TJSONArray>;
    function ExecuteScalar(const SQL: string; const Params: TArray<Variant> = nil): Variant;
    function ExecuteNonQuery(const SQL: string; const Params: TArray<Variant> = nil): Integer;
    function ExecuteBatch(const SQLStatements: TArray<string>): TArray<Integer>;
    function ExecuteTransaction(const SQLStatements: TArray<string>): Boolean;
    function ExecuteStoredProcedure(const ProcName: string; const Params: TArray<Variant> = nil): TJSONArray;
    
    // Table Management
    function CreateTable(const TableSchema: TTableSchema): Boolean;
    function DropTable(const TableName: string): Boolean;
    function AlterTable(const TableName: string; const AlterSQL: string): Boolean;
    function TableExists(const TableName: string): Boolean;
    function GetTableSchema(const TableName: string): TTableSchema;
    function GetTableList: TArray<string>;
    function GetTableRowCount(const TableName: string): Int64;
    function TruncateTable(const TableName: string): Boolean;
    function RenameTable(const OldName, NewName: string): Boolean;
    
    // Index Management
    function CreateIndex(const TableName, IndexName: string; const Columns: TArray<string>; 
      IsUnique: Boolean = False): Boolean;
    function DropIndex(const IndexName: string): Boolean;
    function RebuildIndex(const IndexName: string): Boolean;
    function GetIndexList(const TableName: string = ''): TArray<string>;
    function AnalyzeIndexUsage(const TableName: string): TJSONObject;
    
    // Data Operations
    function InsertRecord(const TableName: string; const Data: TJSONObject): Boolean; overload;
    function InsertRecord(const TableName: string; const Columns: TArray<string>; 
      const Values: TArray<Variant>): Boolean; overload;
    function InsertBatch(const TableName: string; const DataArray: TJSONArray): Integer;
    function UpdateRecord(const TableName: string; const Data: TJSONObject; 
      const WhereClause: string; const WhereParams: TArray<Variant> = nil): Integer;
    function DeleteRecord(const TableName: string; const WhereClause: string; 
      const WhereParams: TArray<Variant> = nil): Integer;
    function SelectRecords(const TableName: string; const Columns: TArray<string> = nil; 
      const WhereClause: string = ''; const WhereParams: TArray<Variant> = nil;
      const OrderBy: string = ''; const Limit: Integer = 0): TJSONArray;
    
    // Training Data Management
    function SaveTrainingSession(const SessionData: TJSONObject): string;
    function LoadTrainingSession(const SessionID: string): TJSONObject;
    function GetTrainingSessions(const ModelName: string = ''): TJSONArray;
    function DeleteTrainingSession(const SessionID: string): Boolean;
    function SaveTrainingMetrics(const SessionID: string; const Metrics: TJSONObject): Boolean;
    function GetTrainingMetrics(const SessionID: string): TJSONArray;
    function SaveModelInfo(const ModelData: TJSONObject): Boolean;
    function GetModelHistory(const ModelName: string): TJSONArray;
    
    // Backup and Restore
    function CreateBackup(BackupType: TBackupType = btFull; const BackupName: string = ''): string;
    function RestoreBackup(const BackupFile: string): Boolean;
    function GetBackupList: TArray<TBackupInfo>;
    function DeleteBackup(const BackupID: string): Boolean;
    function VerifyBackup(const BackupFile: string): Boolean;
    function ScheduleBackup(const Schedule: string; BackupType: TBackupType = btFull): Boolean;
    function GetBackupInfo(const BackupID: string): TBackupInfo;
    
    // Migration and Versioning
    function ApplyMigration(const Migration: TMigrationScript): Boolean;
    function RollbackMigration(const Version: string): Boolean;
    function GetMigrationHistory: TArray<TMigrationScript>;
    function GetCurrentVersion: string;
    function UpdateDatabaseVersion(const NewVersion: string): Boolean;
    function LoadMigrationsFromPath(const MigrationsPath: string): Boolean;
    function GenerateMigration(const FromVersion, ToVersion: string): TMigrationScript;
    
    // Performance and Optimization
    function AnalyzePerformance: TJSONObject;
    function GetSlowQueries(const ThresholdSeconds: Double = 1.0): TJSONArray;
    function OptimizeQuery(const SQL: string; out OptimizedSQL: string): Boolean;
    function GetQueryExecutionPlan(const SQL: string): string;
    function GetDatabaseStatistics: TJSONObject;
    function FindUnusedIndexes: TArray<string>;
    function SuggestIndexes: TArray<string>;
    function AnalyzeFragmentation: TJSONObject;
    
    // Cache Management
    function GetCacheStatistics: TJSONObject;
    function ClearCache: Boolean;
    function WarmupCache(const Queries: TArray<string>): Boolean;
    function SetCachePolicy(const TableName: string; const Policy: TJSONObject): Boolean;
    
    // Import/Export
    function ExportTable(const TableName: string; const FileName: string; 
      const Format: string = 'json'): Boolean;
    function ImportTable(const TableName: string; const FileName: string; 
      const Format: string = 'json'): Boolean;
    function ExportDatabase(const FileName: string; const Format: string = 'sql'): Boolean;
    function ImportDatabase(const FileName: string): Boolean;
    
    // Security and Access Control
    function CreateUser(const Username, Password: string; const Permissions: TArray<string>): Boolean;
    function DropUser(const Username: string): Boolean;
    function GrantPermission(const Username: string; const Permission: string; 
      const TableName: string = ''): Boolean;
    function RevokePermission(const Username: string; const Permission: string; 
      const TableName: string = ''): Boolean;
    function GetUserPermissions(const Username: string): TJSONArray;
    function AuditDataAccess(const Username: string; const Action: string; 
      const TableName: string; const Details: string = ''): Boolean;
    
    // Monitoring and Diagnostics
    function GetConnectionStatus: TJSONObject;
    function GetSystemHealth: TJSONObject;
    function GetLockInformation: TJSONArray;
    function GetActiveQueries: TJSONArray;
    function KillQuery(const QueryID: string): Boolean;
    function GetDatabaseMetrics: TJSONObject;
    function RunDiagnostics: TJSONArray;
    
    // Utilities
    function GenerateID: string;
    function ValidateSQL(const SQL: string): TArray<string>;
    function FormatSQL(const SQL: string): string;
    function MinifySQL(const SQL: string): string;
    function GetTableDDL(const TableName: string): string;
    function CompareSchemas(const Schema1, Schema2: TTableSchema): TJSONObject;
    procedure StartMaintenanceMode;
    procedure EndMaintenanceMode;
    function IsMaintenanceMode: Boolean;
  end;

  TConnectionPool = class
  private
    FConnections: TList<TFDConnection>;
    FAvailableConnections: TQueue<TFDConnection>;
    FConnectionInfo: TDatabaseConnection;
    FMaxSize: Integer;
    FCurrentSize: Integer;
    FCriticalSection: TCriticalSection;
    
  public
    constructor Create(const ConnectionInfo: TDatabaseConnection; MaxSize: Integer = 10);
    destructor Destroy; override;
    
    function GetConnection: TFDConnection;
    procedure ReleaseConnection(Connection: TFDConnection);
    function GetAvailableCount: Integer;
    function GetActiveCount: Integer;
    procedure CloseAllConnections;
  end;

implementation

uses
  System.Hash, System.NetEncoding, System.RegularExpressions, System.Zip,
  Winapi.Windows;

{ TDatabaseManager }

constructor TDatabaseManager.Create;
begin
  inherited Create;
  
  FConnections := TDictionary<string, TDatabaseConnection>.Create;
  FConnectionPools := TDictionary<string, TConnectionPool>.Create;
  FSchemas := TDictionary<string, TTableSchema>.Create;
  FBackups := TList<TBackupInfo>.Create;
  FMigrations := TList<TMigrationScript>.Create;
  FQueryOptimizations := TDictionary<string, TQueryOptimization>.Create;
  FQueryCache := TDictionary<string, TJSONArray>.Create;
  
  // Initialize FireDAC components
  FFDManager := TFDManager.Create(nil);
  FFDPhysSQLiteDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  FFDGUIxWaitCursor := TFDGUIxWaitCursor.Create(nil);
  FMainConnection := TFDConnection.Create(nil);
  
  FShutdownEvent := TEvent.Create(nil, True, False, '');
  
  // Default settings
  FDatabasePath := TPath.Combine(TPath.GetDocumentsPath, 'OllamaTrainer', 'Database');
  FMaxConnections := 10;
  FConnectionTimeout := 30;
  FQueryTimeout := 60;
  FAutoBackup := True;
  FBackupInterval := 24; // 24 hours
  FCompressionLevel := 6;
  FEncryptionEnabled := False;
  FQueryLogging := True;
  FPerformanceMonitoring := True;
  FAutoOptimize := True;
  FCacheEnabled := True;
  FCacheTimeout := 30; // 30 minutes
  FCacheMaxSize := 100; // 100 MB
  FCacheHitCount := 0;
  FCacheMissCount := 0;
  
  ForceDirectories(FDatabasePath);
end;

destructor TDatabaseManager.Destroy;
var
  Pool: TConnectionPool;
  Backup: TBackupInfo;
  Migration: TMigrationScript;
begin
  // Signal shutdown
  FShutdownEvent.SetEvent;
  
  // Wait for threads to finish
  if Assigned(FBackupThread) then
  begin
    FBackupThread.WaitFor;
    FBackupThread.Free;
  end;
  
  if Assigned(FMaintenanceThread) then
  begin
    FMaintenanceThread.WaitFor;
    FMaintenanceThread.Free;
  end;
  
  // Close all connections
  CloseAllConnections;
  
  // Free connection pools
  for var Pair in FConnectionPools do
  begin
    Pool := Pair.Value;
    Pool.Free;
  end;
  
  // Free backup info metadata
  for var I := 0 to FBackups.Count - 1 do
  begin
    Backup := FBackups[I];
    if Assigned(Backup.Metadata) then
      Backup.Metadata.Free;
  end;
  
  // Free query cache
  for var Pair in FQueryCache do
    Pair.Value.Free;
  
  // Free FireDAC components
  FMainConnection.Free;
  FFDGUIxWaitCursor.Free;
  FFDPhysSQLiteDriverLink.Free;
  FFDManager.Free;
  
  FShutdownEvent.Free;
  FQueryCache.Free;
  FQueryOptimizations.Free;
  FMigrations.Free;
  FBackups.Free;
  FSchemas.Free;
  FConnectionPools.Free;
  FConnections.Free;
  
  inherited Destroy;
end;

function TDatabaseManager.Initialize(const DatabasePath: string): Boolean;
var
  DBPath: string;
begin
  Result := False;
  
  try
    if DatabasePath <> '' then
      FDatabasePath := DatabasePath;
    
    DBPath := TPath.Combine(FDatabasePath, 'ollama_trainer.db');
    
    // Configure main connection
    FMainConnection.Close;
    FMainConnection.DriverName := 'SQLite';
    FMainConnection.Params.Clear;
    FMainConnection.Params.Add('Database=' + DBPath);
    FMainConnection.Params.Add('LockingMode=Normal');
    FMainConnection.Params.Add('Synchronous=Normal');
    FMainConnection.Params.Add('JournalMode=WAL');
    FMainConnection.Params.Add('CacheSize=10000');
    FMainConnection.Params.Add('PageSize=4096');
    
    if FEncryptionEnabled and (FEncryptionKey <> '') then
      FMainConnection.Params.Add('Password=' + FEncryptionKey);
    
    FMainConnection.LoginPrompt := False;
    FMainConnection.Open;
    
    Result := InitializeDatabase;
    
    if Result then
    begin
      // Start background threads
      if FAutoBackup then
      begin
        FBackupThread := TThread.CreateAnonymousThread(BackupThreadExecute);
        FBackupThread.Start;
      end;
      
      FMaintenanceThread := TThread.CreateAnonymousThread(MaintenanceThreadExecute);
      FMaintenanceThread.Start;
      
      if Assigned(FOnDatabaseEvent) then
      begin
        var EventData := TJSONObject.Create;
        EventData.AddPair('database_path', DBPath);
        EventData.AddPair('initialized_at', DateToISO8601(Now));
        FOnDatabaseEvent(Self, 'database_initialized', EventData);
      end;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(FOnDatabaseEvent) then
      begin
        var EventData := TJSONObject.Create;
        EventData.AddPair('error', E.Message);
        FOnDatabaseEvent(Self, 'initialization_error', EventData);
      end;
    end;
  end;
end;

function TDatabaseManager.InitializeDatabase: Boolean;
begin
  Result := False;
  
  try
    Result := CreateSystemTables;
    
    if Result then
    begin
      // Load existing migrations
      LoadMigrationsFromPath(TPath.Combine(FDatabasePath, 'migrations'));
      
      // Apply pending migrations
      var CurrentVersion := GetCurrentVersion;
      if CurrentVersion = '' then
        UpdateDatabaseVersion('1.0.0');
    end;
    
  except
    on E: Exception do
    begin
      // Log error
      Result := False;
    end;
  end;
end;

function TDatabaseManager.CreateSystemTables: Boolean;
const
  SystemTables: array[0..5] of string = (
    // Training Sessions table
    '''CREATE TABLE IF NOT EXISTS training_sessions (
      id TEXT PRIMARY KEY,
      model_name TEXT NOT NULL,
      session_name TEXT,
      start_time DATETIME,
      end_time DATETIME,
      status TEXT,
      parameters TEXT,
      results TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )''',
    
    // Training Metrics table
    '''CREATE TABLE IF NOT EXISTS training_metrics (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT NOT NULL,
      epoch INTEGER,
      training_loss REAL,
      validation_loss REAL,
      training_accuracy REAL,
      validation_accuracy REAL,
      learning_rate REAL,
      time_elapsed REAL,
      memory_usage INTEGER,
      recorded_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (session_id) REFERENCES training_sessions(id)
    )''',
    
    // Model Information table
    '''CREATE TABLE IF NOT EXISTS model_info (
      id TEXT PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      type TEXT,
      size INTEGER,
      parameters INTEGER,
      accuracy REAL,
      created_at DATETIME,
      modified_at DATETIME,
      description TEXT,
      metadata TEXT
    )''',
    
    // Database Migrations table
    '''CREATE TABLE IF NOT EXISTS database_migrations (
      version TEXT PRIMARY KEY,
      name TEXT,
      description TEXT,
      up_script TEXT,
      down_script TEXT,
      applied_at DATETIME,
      execution_time REAL,
      checksum TEXT
    )''',
    
    // Query Performance table
    '''CREATE TABLE IF NOT EXISTS query_performance (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      query_hash TEXT,
      sql_query TEXT,
      execution_time REAL,
      records_affected INTEGER,
      executed_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      connection_id TEXT
    )''',
    
    // System Configuration table
    '''CREATE TABLE IF NOT EXISTS system_config (
      key TEXT PRIMARY KEY,
      value TEXT,
      description TEXT,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )'''
  );
  
  Indexes: array[0..4] of string = (
    'CREATE INDEX IF NOT EXISTS idx_training_sessions_model ON training_sessions(model_name)',
    'CREATE INDEX IF NOT EXISTS idx_training_metrics_session ON training_metrics(session_id)',
    'CREATE INDEX IF NOT EXISTS idx_training_metrics_epoch ON training_metrics(session_id, epoch)',
    'CREATE INDEX IF NOT EXISTS idx_model_info_name ON model_info(name)',
    'CREATE INDEX IF NOT EXISTS idx_query_performance_hash ON query_performance(query_hash)'
  );

var
  I: Integer;
begin
  Result := False;
  
  try
    FMainConnection.StartTransaction;
    
    // Create tables
    for I := 0 to High(SystemTables) do
      FMainConnection.ExecSQL(SystemTables[I]);
    
    // Create indexes
    for I := 0 to High(Indexes) do
      FMainConnection.ExecSQL(Indexes[I]);
    
    FMainConnection.Commit;
    Result := True;
    
  except
    on E: Exception do
    begin
      FMainConnection.Rollback;
      raise Exception.Create('Failed to create system tables: ' + E.Message);
    end;
  end;
end;

function TDatabaseManager.ExecuteQuery(const SQL: string; Format: TQueryResultFormat): Variant;
begin
  Result := ExecuteQuery(SQL, nil, Format);
end;

function TDatabaseManager.ExecuteQuery(const SQL: string; const Params: TArray<Variant>; 
  Format: TQueryResultFormat): Variant;
var
  CacheKey: string;
  CachedResult: TJSONArray;
  StartTime: TDateTime;
  ExecutionTime: Double;
begin
  Result := Null;
  
  // Check cache first
  if FCacheEnabled and (Format = qrfJSON) then
  begin
    CacheKey := GetCacheKey(SQL, Params);
    if FQueryCache.TryGetValue(CacheKey, CachedResult) and IsCacheValid(CacheKey) then
    begin
      Inc(FCacheHitCount);
      Result := CachedResult.Clone;
      Exit;
    end;
    Inc(FCacheMissCount);
  end;
  
  StartTime := Now;
  
  try
    Result := ExecuteQueryInternal(SQL, Params, FMainConnection, Format);
    
    ExecutionTime := (Now - StartTime) * SecsPerDay;
    
    // Cache result if enabled
    if FCacheEnabled and (Format = qrfJSON) and VarIsType(Result, varUnknown) then
    begin
      var JSONResult := TJSONArray(TVarData(Result).VUnknown);
      if Assigned(JSONResult) then
        FQueryCache.AddOrSetValue(CacheKey, JSONResult.Clone as TJSONArray);
    end;
    
    // Log query if enabled
    if FQueryLogging then
      LogQuery(SQL, ExecutionTime, 0); // Records affected would need to be calculated
    
    // Fire event
    if Assigned(FOnQueryExecuted) then
      FOnQueryExecuted(Self, SQL, ExecutionTime, 0);
      
  except
    on E: Exception do
    begin
      ExecutionTime := (Now - StartTime) * SecsPerDay;
      LogQuery(SQL + ' [ERROR: ' + E.Message + ']', ExecutionTime, -1);
      raise;
    end;
  end;
end;

function TDatabaseManager.ExecuteQueryInternal(const SQL: string; const Params: TArray<Variant>;
  Connection: TFDConnection; Format: TQueryResultFormat): Variant;
var
  Query: TFDQuery;
  I: Integer;
begin
  Result := Null;
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := SQL;
    
    // Set parameters
    if Length(Params) > 0 then
    begin
      for I := 0 to High(Params) do
        Query.Params[I].Value := Params[I];
    end;
    
    Query.Open;
    
    case Format of
      qrfDataSet: Result := Query; // Caller must free
      qrfJSON: Result := ConvertDataSetToJSON(Query);
      qrfXML: Result := ConvertDataSetToXML(Query);
      qrfCSV: Result := ConvertDataSetToCSV(Query);
      qrfArray: Result := ConvertDataSetToArray(Query);
    end;
    
  finally
    if Format <> qrfDataSet then
      Query.Free;
  end;
end;

function TDatabaseManager.ConvertDataSetToJSON(DataSet: TDataSet): TJSONArray;
var
  JSONObject: TJSONObject;
  Field: TField;
  I: Integer;
begin
  Result := TJSONArray.Create;
  
  DataSet.First;
  while not DataSet.Eof do
  begin
    JSONObject := TJSONObject.Create;
    
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[I];
      
      if Field.IsNull then
        JSONObject.AddPair(Field.FieldName, TJSONNull.Create)
      else
      begin
        case Field.DataType of
          ftString, ftWideString, ftMemo, ftWideMemo:
            JSONObject.AddPair(Field.FieldName, Field.AsString);
          ftInteger, ftSmallint, ftWord, ftLargeint:
            JSONObject.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsInteger));
          ftFloat, ftCurrency, ftBCD:
            JSONObject.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsFloat));
          ftBoolean:
            JSONObject.AddPair(Field.FieldName, TJSONBool.Create(Field.AsBoolean));
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            JSONObject.AddPair(Field.FieldName, DateToISO8601(Field.AsDateTime));
        else
          JSONObject.AddPair(Field.FieldName, Field.AsString);
        end;
      end;
    end;
    
    Result.AddElement(JSONObject);
    DataSet.Next;
  end;
end;

function TDatabaseManager.SaveTrainingSession(const SessionData: TJSONObject): string;
var
  SessionID: string;
  SQL: string;
  Query: TFDQuery;
begin
  SessionID := GenerateID;
  
  SQL := '''INSERT INTO training_sessions 
    (id, model_name, session_name, start_time, status, parameters, created_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)''';
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FMainConnection;
    Query.SQL.Text := SQL;
    
    Query.Params[0].AsString := SessionID;
    Query.Params[1].AsString := SessionData.GetValue<string>('model_name', '');
    Query.Params[2].AsString := SessionData.GetValue<string>('session_name', '');
    Query.Params[3].AsDateTime := Now;
    Query.Params[4].AsString := 'running';
    Query.Params[5].AsString := SessionData.GetValue('parameters', TJSONObject.Create).ToString;
    Query.Params[6].AsDateTime := Now;
    
    Query.ExecSQL;
    
  finally
    Query.Free;
  end;
  
  Result := SessionID;
  
  if Assigned(FOnDatabaseEvent) then
  begin
    var EventData := TJSONObject.Create;
    EventData.AddPair('session_id', SessionID);
    EventData.AddPair('action', 'training_session_created');
    FOnDatabaseEvent(Self, 'training_session_event', EventData);
  end;
end;

function TDatabaseManager.SaveTrainingMetrics(const SessionID: string; const Metrics: TJSONObject): Boolean;
var
  SQL: string;
  Query: TFDQuery;
begin
  Result := False;
  
  SQL := '''INSERT INTO training_metrics 
    (session_id, epoch, training_loss, validation_loss, training_accuracy, 
     validation_accuracy, learning_rate, time_elapsed, memory_usage)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)''';
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FMainConnection;
    Query.SQL.Text := SQL;
    
    Query.Params[0].AsString := SessionID;
    Query.Params[1].AsInteger := Metrics.GetValue<Integer>('epoch', 0);
    Query.Params[2].AsFloat := Metrics.GetValue<Double>('training_loss', 0.0);
    Query.Params[3].AsFloat := Metrics.GetValue<Double>('validation_loss', 0.0);
    Query.Params[4].AsFloat := Metrics.GetValue<Double>('training_accuracy', 0.0);
    Query.Params[5].AsFloat := Metrics.GetValue<Double>('validation_accuracy', 0.0);
    Query.Params[6].AsFloat := Metrics.GetValue<Double>('learning_rate', 0.0);
    Query.Params[7].AsFloat := Metrics.GetValue<Double>('time_elapsed', 0.0);
    Query.Params[8].AsLargeInt := Metrics.GetValue<Int64>('memory_usage', 0);
    
    Query.ExecSQL;
    Result := True;
    
  except
    on E: Exception do
    begin
      // Log error
      Result := False;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.CreateBackup(BackupType: TBackupType; const BackupName: string): string;
var
  BackupInfo: TBackupInfo;
  BackupFileName: string;
  StartTime: TDateTime;
begin
  StartTime := Now;
  
  BackupInfo.BackupID := GenerateID;
  BackupInfo.BackupType := BackupType;
  BackupInfo.CreatedAt := StartTime;
  BackupInfo.Success := False;
  BackupInfo.Metadata := TJSONObject.Create;
  
  try
    if BackupName <> '' then
      BackupFileName := BackupName
    else
      BackupFileName := CreateBackupFileName(BackupType);
    
    BackupInfo.FileName := ExtractFileName(BackupFileName);
    BackupInfo.FilePath := BackupFileName;
    
    if Assigned(FOnBackupProgress) then
      FOnBackupProgress(Self, 10, 'Iniciando backup...');
    
    // Create backup based on type
    case BackupType of
      btFull: 
      begin
        // Full database backup
        FMainConnection.ExecSQL('VACUUM INTO ''' + BackupFileName + '''');
      end;
      btIncremental:
      begin
        // Incremental backup (simplified - would need WAL file handling)
        FMainConnection.ExecSQL('VACUUM INTO ''' + BackupFileName + '''');
      end;
    end;
    
    if Assigned(FOnBackupProgress) then
      FOnBackupProgress(Self, 60, 'Backup criado, processando...');
    
    BackupInfo.FileSize := TFile.GetSize(BackupFileName);
    
    // Compress if enabled
    if FCompressionLevel > 0 then
    begin
      if Assigned(FOnBackupProgress) then
        FOnBackupProgress(Self, 70, 'Comprimindo backup...');
      BackupInfo.Compressed := CompressBackup(BackupFileName);
    end;
    
    // Encrypt if enabled
    if FEncryptionEnabled then
    begin
      if Assigned(FOnBackupProgress) then
        FOnBackupProgress(Self, 80, 'Criptografando backup...');
      BackupInfo.Encrypted := EncryptBackup(BackupFileName);
    end;
    
    // Calculate checksum
    if Assigned(FOnBackupProgress) then
      FOnBackupProgress(Self, 90, 'Calculando checksum...');
    BackupInfo.CheckSum := CalculateCheckSum(BackupFileName);
    
    BackupInfo.CompletedAt := Now;
    BackupInfo.Duration := BackupInfo.CompletedAt - StartTime;
    BackupInfo.Success := True;
    
    // Add metadata
    BackupInfo.Metadata.AddPair('database_size', TJSONNumber.Create(GetDatabaseSize));
    BackupInfo.Metadata.AddPair('compression_enabled', TJSONBool.Create(BackupInfo.Compressed));
    BackupInfo.Metadata.AddPair('encryption_enabled', TJSONBool.Create(BackupInfo.Encrypted));
    BackupInfo.Metadata.AddPair('backup_tool', 'OllamaTrainer DatabaseManager');
    
    FBackups.Add(BackupInfo);
    
    if Assigned(FOnBackupProgress) then
      FOnBackupProgress(Self, 100, 'Backup concluído com sucesso');
    
    Result := BackupInfo.BackupID;
    
  except
    on E: Exception do
    begin
      BackupInfo.Success := False;
      BackupInfo.ErrorMessage := E.Message;
      BackupInfo.CompletedAt := Now;
      BackupInfo.Duration := BackupInfo.CompletedAt - StartTime;
      
      FBackups.Add(BackupInfo);
      
      if Assigned(FOnBackupProgress) then
        FOnBackupProgress(Self, 0, 'Erro no backup: ' + E.Message);
      
      Result := '';
    end;
  end;
end;

// Implementações básicas para métodos auxiliares

function TDatabaseManager.GenerateID: string;
begin
  Result := 'id_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + 
    IntToStr(Random(1000)).PadLeft(3, '0');
end;

function TDatabaseManager.CreateBackupFileName(BackupType: TBackupType): string;
var
  TypeStr: string;
begin
  case BackupType of
    btFull: TypeStr := 'full';
    btIncremental: TypeStr := 'incr';
    btDifferential: TypeStr := 'diff';
    btTransaction: TypeStr := 'tran';
  end;
  
  Result := TPath.Combine(FDatabasePath, 'backups', 
    Format('backup_%s_%s.db', [TypeStr, FormatDateTime('yyyymmdd_hhnnss', Now)]));
    
  ForceDirectories(ExtractFilePath(Result));
end;

function TDatabaseManager.HashQuery(const SQL: string): string;
begin
  Result := THashMD5.GetHashString(LowerCase(Trim(SQL)));
end;

function TDatabaseManager.GetCacheKey(const SQL: string; const Params: TArray<Variant>): string;
var
  ParamsStr: string;
  I: Integer;
begin
  ParamsStr := '';
  for I := 0 to High(Params) do
  begin
    if I > 0 then
      ParamsStr := ParamsStr + '|';
    ParamsStr := ParamsStr + VarToStr(Params[I]);
  end;
  
  Result := THashMD5.GetHashString(SQL + '::' + ParamsStr);
end;

function TDatabaseManager.IsCacheValid(const CacheKey: string): Boolean;
begin
  // Simplified cache validation - in real implementation would check timestamps
  Result := True;
end;

procedure TDatabaseManager.LogQuery(const SQL: string; ExecutionTime: Double; RecordsAffected: Integer);
var
  LogSQL: string;
  Query: TFDQuery;
begin
  if not FQueryLogging then
    Exit;
    
  LogSQL := '''INSERT INTO query_performance 
    (query_hash, sql_query, execution_time, records_affected, connection_id)
    VALUES (?, ?, ?, ?, ?)''';
  
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FMainConnection;
    Query.SQL.Text := LogSQL;
    
    Query.Params[0].AsString := HashQuery(SQL);
    Query.Params[1].AsString := SQL;
    Query.Params[2].AsFloat := ExecutionTime;
    Query.Params[3].AsInteger := RecordsAffected;
    Query.Params[4].AsString := 'main';
    
    Query.ExecSQL;
    
  except
    // Ignore logging errors to prevent infinite loops
  finally
    Query.Free;
  end;
end;

// Threading procedures
procedure TDatabaseManager.BackupThreadExecute;
begin
  while not FShutdownEvent.WaitFor(FBackupInterval * 60 * 60 * 1000) do // Convert hours to milliseconds
  begin
    if FAutoBackup then
    begin
      try
        CreateBackup(btIncremental);
      except
        // Log error but continue
      end;
    end;
  end;
end;

procedure TDatabaseManager.MaintenanceThreadExecute;
begin
  while not FShutdownEvent.WaitFor(60 * 60 * 1000) do // Run every hour
  begin
    try
      if FAutoOptimize then
        OptimizeDatabase;
        
      CleanupCache;
      
      // Cleanup old performance logs (keep last 30 days)
      var CleanupSQL := 'DELETE FROM query_performance WHERE executed_at < datetime(''now'', ''-30 days'')';
      FMainConnection.ExecSQL(CleanupSQL);
      
    except
      // Log error but continue
    end;
  end;
end;

// Implementações placeholder para métodos restantes
function TDatabaseManager.ConvertDataSetToXML(DataSet: TDataSet): string; begin Result := '<xml></xml>'; end;
function TDatabaseManager.ConvertDataSetToCSV(DataSet: TDataSet): string; begin Result := 'CSV data'; end;
function TDatabaseManager.ConvertDataSetToArray(DataSet: TDataSet): TArray<TArray<Variant>>; begin SetLength(Result, 0); end;
function TDatabaseManager.CompressBackup(const FileName: string): Boolean; begin Result := True; end;
function TDatabaseManager.EncryptBackup(const FileName: string): Boolean; begin Result := True; end;
function TDatabaseManager.CalculateCheckSum(const FileName: string): string; begin Result := 'checksum'; end;
procedure TDatabaseManager.CleanupCache; begin end;
procedure TDatabaseManager.OptimizeDatabase; begin end;
procedure TDatabaseManager.AnalyzeTableStatistics; begin end;
procedure TDatabaseManager.RebuildIndexes; begin end;
procedure TDatabaseManager.InvalidateCache(const Pattern: string); begin end;
procedure TDatabaseManager.UpdateStatistics(const OperationType: string; const Metadata: TJSONObject); begin end;

// TConnectionPool implementation
constructor TConnectionPool.Create(const ConnectionInfo: TDatabaseConnection; MaxSize: Integer);
begin
  inherited Create;
  
  FConnectionInfo := ConnectionInfo;
  FMaxSize := MaxSize;
  FCurrentSize := 0;
  FConnections := TList<TFDConnection>.Create;
  FAvailableConnections := TQueue<TFDConnection>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TConnectionPool.Destroy;
begin
  CloseAllConnections;
  FCriticalSection.Free;
  FAvailableConnections.Free;
  FConnections.Free;
  inherited Destroy;
end;

function TConnectionPool.GetConnection: TFDConnection;
begin
  Result := nil;
  
  FCriticalSection.Acquire;
  try
    if FAvailableConnections.Count > 0 then
      Result := FAvailableConnections.Dequeue
    else if FCurrentSize < FMaxSize then
    begin
      // Create new connection
      Result := TFDConnection.Create(nil);
      Result.DriverName := 'SQLite';
      // Configure connection parameters here
      FConnections.Add(Result);
      Inc(FCurrentSize);
    end;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TConnectionPool.ReleaseConnection(Connection: TFDConnection);
begin
  if not Assigned(Connection) then
    Exit;
    
  FCriticalSection.Acquire;
  try
    FAvailableConnections.Enqueue(Connection);
  finally
    FCriticalSection.Release;
  end;
end;

function TConnectionPool.GetAvailableCount: Integer;
begin
  FCriticalSection.Acquire;
  try
    Result := FAvailableConnections.Count;
  finally
    FCriticalSection.Release;
  end;
end;

function TConnectionPool.GetActiveCount: Integer;
begin
  FCriticalSection.Acquire;
  try
    Result := FCurrentSize - FAvailableConnections.Count;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TConnectionPool.CloseAllConnections;
var
  Connection: TFDConnection;
begin
  FCriticalSection.Acquire;
  try
    // Close all connections
    for Connection in FConnections do
    begin
      try
        Connection.Close;
        Connection.Free;
      except
        // Ignore errors during cleanup
      end;
    end;
    
    FConnections.Clear;
    FAvailableConnections.Clear;
    FCurrentSize := 0;
  finally
    FCriticalSection.Release;
  end;
end;

end.