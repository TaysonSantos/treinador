unit ValidationTests;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.DateUtils, System.Math, System.RegularExpressions, System.Threading,
  System.IOUtils, System.Variants, System.Rtti, OllamaAPI, TrainingEngine,
  ModelManager, DataProcessor, DatabaseManager, Logger;

type
  TTestType = (ttUnit, ttIntegration, ttPerformance, ttSecurity, ttCompatibility, 
    ttRegression, ttStress, ttValidation, ttCompliance);
    
  TTestStatus = (tsNotRun, tsRunning, tsPassed, tsFailed, tsSkipped, tsError, 
    tsTimeout, tsInconclusive);
    
  TTestPriority = (tpLow, tpMedium, tpHigh, tpCritical);
  
  TTestSeverity = (tsMinor, tsMajor, tsCritical, tsBlocker);
  
  TTestResult = record
    TestID: string;
    TestName: string;
    TestType: TTestType;
    Status: TTestStatus;
    Priority: TTestPriority;
    Severity: TTestSeverity;
    StartTime: TDateTime;
    EndTime: TDateTime;
    Duration: TDateTime;
    ExpectedResult: Variant;
    ActualResult: Variant;
    ErrorMessage: string;
    StackTrace: string;
    Metadata: TJSONObject;
    Assertions: TArray<record
      Name: string;
      Expected: Variant;
      Actual: Variant;
      Passed: Boolean;
      Message: string;
    end>;
    TestData: TJSONObject;
    Environment: TJSONObject;
    Dependencies: TArray<string>;
    Coverage: Double;
    MemoryUsage: Int64;
    CPUUsage: Double;
    Screenshots: TArray<string>;
    LogEntries: TArray<string>;
  end;
  
  TTestSuite = record
    SuiteID: string;
    Name: string;
    Description: string;
    TestTypes: TArray<TTestType>;
    Tests: TArray<TTestResult>;
    SetupProcedure: TProc;
    TeardownProcedure: TProc;
    BeforeEachTest: TProc<string>;
    AfterEachTest: TProc<string, TTestResult>;
    Timeout: Integer; // seconds
    RetryCount: Integer;
    Parallel: Boolean;
    Dependencies: TArray<string>;
    Tags: TArray<string>;
    Configuration: TJSONObject;
    CreatedAt: TDateTime;
    LastRun: TDateTime;
  end;
  
  TValidationRule = record
    RuleID: string;
    Name: string;
    Description: string;
    Category: string;
    RuleType: string; // format, range, required, custom
    Condition: string;
    ErrorMessage: string;
    WarningMessage: string;
    Enabled: Boolean;
    Severity: TTestSeverity;
    ValidationFunction: TFunc<Variant, Boolean>;
    Parameters: TJSONObject;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
  end;
  
  TValidationResult = record
    IsValid: Boolean;
    Errors: TArray<string>;
    Warnings: TArray<string>;
    ValidationTime: TDateTime;
    ValidatedFields: Integer;
    FailedRules: TArray<string>;
    PassedRules: TArray<string>;
    Details: TJSONObject;
  end;
  
  TPerformanceBenchmark = record
    BenchmarkID: string;
    Name: string;
    Description: string;
    Category: string;
    BaselineValue: Double;
    CurrentValue: Double;
    Unit_: string;
    ImprovementThreshold: Double; // percentage
    RegressionThreshold: Double; // percentage
    MeasurementFunction: TFunc<Double>;
    TestData: TJSONObject;
    History: TArray<record
      Timestamp: TDateTime;
      Value: Double;
      Environment: string;
      Version: string;
    end>;
    CreatedAt: TDateTime;
    LastMeasured: TDateTime;
  end;
  
  TTestEvent = procedure(Sender: TObject; const TestResult: TTestResult) of object;
  TSuiteEvent = procedure(Sender: TObject; const SuiteID: string; 
    const Results: TArray<TTestResult>) of object;
  TValidationEvent = procedure(Sender: TObject; const ValidationResult: TValidationResult) of object;
  TBenchmarkEvent = procedure(Sender: TObject; const Benchmark: TPerformanceBenchmark) of object;
  
  TValidationTests = class
  private
    FTestSuites: TDictionary<string, TTestSuite>;
    FTestResults: TList<TTestResult>;
    FValidationRules: TDictionary<string, TValidationRule>;
    FBenchmarks: TDictionary<string, TPerformanceBenchmark>;
    FRunningTests: TDictionary<string, ITask>;
    
    // Component references
    FOllamaAPI: TOllamaAPI;
    FTrainingEngine: TTrainingEngine;
    FModelManager: TModelManager;
    FDataProcessor: TDataProcessor;
    FDatabaseManager: TDatabaseManager;
    FLogger: TLogger;
    
    // Configuration
    FEnabled: Boolean;
    FMaxConcurrentTests: Integer;
    FDefaultTimeout: Integer;
    FRetryFailedTests: Boolean;
    FDefaultRetryCount: Integer;
    FGenerateReports: Boolean;
    FReportPath: string;
    FContinueOnFailure: Boolean;
    FVerboseLogging: Boolean;
    FCollectCoverage: Boolean;
    FCollectPerformanceMetrics: Boolean;
    
    // Events
    FOnTestStarted: TTestEvent;
    FOnTestCompleted: TTestEvent;
    FOnSuiteCompleted: TSuiteEvent;
    FOnValidationCompleted: TValidationEvent;
    FOnBenchmarkCompleted: TBenchmarkEvent;
    
    // Assertion helpers
    function CreateAssertion(const Name: string; const Expected, Actual: Variant; 
      const Message: string = ''): Boolean;
    procedure AssertEquals(const Expected, Actual: Variant; const Message: string = '');
    procedure AssertNotEquals(const Expected, Actual: Variant; const Message: string = '');
    procedure AssertTrue(const Condition: Boolean; const Message: string = '');
    procedure AssertFalse(const Condition: Boolean; const Message: string = '');
    procedure AssertNotNull(const Value: Variant; const Message: string = '');
    procedure AssertNull(const Value: Variant; const Message: string = '');
    procedure AssertGreaterThan(const Expected, Actual: Variant; const Message: string = '');
    procedure AssertLessThan(const Expected, Actual: Variant; const Message: string = '');
    procedure AssertInRange(const Value, Min, Max: Variant; const Message: string = '');
    procedure AssertContains(const Container, Item: string; const Message: string = '');
    procedure AssertMatches(const Pattern, Text: string; const Message: string = '');
    
    // Test execution helpers
    function ExecuteTest(var TestResult: TTestResult): Boolean;
    function RunTestWithTimeout(const TestProc: TProc; TimeoutSeconds: Integer): Boolean;
    function CaptureException(const TestProc: TProc; var ExceptionInfo: string): Boolean;
    function MeasurePerformance(const TestProc: TProc): record
      ExecutionTime: TDateTime;
      MemoryUsage: Int64;
      CPUUsage: Double;
    end;
    procedure CollectTestEnvironment(var Environment: TJSONObject);
    function GenerateTestID: string;
    procedure LogTestResult(const TestResult: TTestResult);
    procedure UpdateTestStatistics(const TestResult: TTestResult);
    
    // Validation helpers
    function ValidateDataType(const Value: Variant; const ExpectedType: string): Boolean;
    function ValidateRange(const Value: Variant; const Min, Max: Variant): Boolean;
    function ValidateFormat(const Value: string; const Pattern: string): Boolean;
    function ValidateRequired(const Value: Variant): Boolean;
    function ValidateCustomRule(const Value: Variant; const Rule: TValidationRule): Boolean;
    
    // Component testing
    function TestOllamaConnection: TTestResult;
    function TestModelLoading: TTestResult;
    function TestDataProcessing: TTestResult;
    function TestDatabaseConnection: TTestResult;
    function TestTrainingPipeline: TTestResult;
    
    // Integration testing
    function TestEndToEndTraining: TTestResult;
    function TestModelManagement: TTestResult;
    function TestDataFlow: TTestResult;
    function TestReportGeneration: TTestResult;
    function TestUserWorkflow: TTestResult;
    
    // Performance testing
    function TestTrainingPerformance: TTestResult;
    function TestModelInferenceSpeed: TTestResult;
    function TestDataProcessingSpeed: TTestResult;
    function TestMemoryUsage: TTestResult;
    function TestConcurrentOperations: TTestResult;
    
    // Security testing
    function TestDataEncryption: TTestResult;
    function TestAccessControl: TTestResult;
    function TestInputValidation: TTestResult;
    function TestSQLInjection: TTestResult;
    function TestFileSystemAccess: TTestResult;
    
    // Regression testing
    function TestBackwardCompatibility: TTestResult;
    function TestConfigurationMigration: TTestResult;
    function TestAPICompatibility: TTestResult;
    function TestModelCompatibility: TTestResult;
    
  public
    constructor Create(OllamaAPI: TOllamaAPI; TrainingEngine: TTrainingEngine;
      ModelManager: TModelManager; DataProcessor: TDataProcessor;
      DatabaseManager: TDatabaseManager; Logger: TLogger);
    destructor Destroy; override;
    
    // Properties
    property Enabled: Boolean read FEnabled write FEnabled;
    property MaxConcurrentTests: Integer read FMaxConcurrentTests write FMaxConcurrentTests;
    property DefaultTimeout: Integer read FDefaultTimeout write FDefaultTimeout;
    property RetryFailedTests: Boolean read FRetryFailedTests write FRetryFailedTests;
    property GenerateReports: Boolean read FGenerateReports write FGenerateReports;
    property ReportPath: string read FReportPath write FReportPath;
    property VerboseLogging: Boolean read FVerboseLogging write FVerboseLogging;
    property CollectCoverage: Boolean read FCollectCoverage write FCollectCoverage;
    
    // Events
    property OnTestStarted: TTestEvent read FOnTestStarted write FOnTestStarted;
    property OnTestCompleted: TTestEvent read FOnTestCompleted write FOnTestCompleted;
    property OnSuiteCompleted: TSuiteEvent read FOnSuiteCompleted write FOnSuiteCompleted;
    property OnValidationCompleted: TValidationEvent read FOnValidationCompleted write FOnValidationCompleted;
    property OnBenchmarkCompleted: TBenchmarkEvent read FOnBenchmarkCompleted write FOnBenchmarkCompleted;
    
    // Test Suite Management
    function CreateTestSuite(const Name, Description: string; TestTypes: TArray<TTestType>): string;
    function AddTestToSuite(const SuiteID: string; const TestProc: TProc<TTestResult>; 
      const TestName: string; TestType: TTestType = ttUnit; Priority: TTestPriority = tpMedium): Boolean;
    function RemoveTestSuite(const SuiteID: string): Boolean;
    function GetTestSuite(const SuiteID: string): TTestSuite;
    function GetTestSuites: TArray<string>;
    function CloneTestSuite(const SourceSuiteID, NewSuiteID: string): Boolean;
    
    // Test Execution
    function RunTest(const TestName: string): TTestResult;
    function RunTestSuite(const SuiteID: string): TArray<TTestResult>;
    function RunAllTests: TArray<TTestResult>;
    function RunTestsByType(TestType: TTestType): TArray<TTestResult>;
    function RunTestsByPriority(Priority: TTestPriority): TArray<TTestResult>;
    function RunTestsAsync(const TestNames: TArray<string>): ITask<TArray<TTestResult>>;
    function RunContinuousTests(const SuiteID: string; IntervalMinutes: Integer): Boolean;
    function StopContinuousTests(const SuiteID: string): Boolean;
    
    // Built-in Test Suites
    function RunSystemValidationTests: TArray<TTestResult>;
    function RunPerformanceTests: TArray<TTestResult>;
    function RunIntegrationTests: TArray<TTestResult>;
    function RunSecurityTests: TArray<TTestResult>;
    function RunRegressionTests: TArray<TTestResult>;
    function RunSmokeTests: TArray<TTestResult>;
    function RunStressTests: TArray<TTestResult>;
    
    // Data Validation
    function ValidateData(const Data: TJSONObject; const Rules: TArray<TValidationRule>): TValidationResult; overload;
    function ValidateData(const Data: Variant; const RuleID: string): TValidationResult; overload;
    function ValidateTrainingData(const DataPath: string): TValidationResult;
    function ValidateModelOutput(const ModelName: string; const TestInput: string): TValidationResult;
    function ValidateConfiguration(const Config: TJSONObject): TValidationResult;
    function ValidateDatabaseSchema: TValidationResult;
    function ValidateAPIEndpoints: TValidationResult;
    
    // Validation Rules Management
    function AddValidationRule(const Rule: TValidationRule): Boolean;
    function RemoveValidationRule(const RuleID: string): Boolean;
    function GetValidationRule(const RuleID: string): TValidationRule;
    function GetValidationRules(const Category: string = ''): TArray<TValidationRule>;
    function EnableValidationRule(const RuleID: string; Enabled: Boolean = True): Boolean;
    function TestValidationRule(const RuleID: string; const TestData: Variant): Boolean;
    
    // Performance Benchmarking
    function CreateBenchmark(const Name, Description: string; 
      const MeasurementFunc: TFunc<Double>): string;
    function RunBenchmark(const BenchmarkID: string): TPerformanceBenchmark;
    function RunAllBenchmarks: TArray<TPerformanceBenchmark>;
    function CompareBenchmarks(const BenchmarkID1, BenchmarkID2: string): TJSONObject;
    function GetBenchmarkHistory(const BenchmarkID: string; Days: Integer = 30): TJSONArray;
    function SetBenchmarkBaseline(const BenchmarkID: string; const BaselineValue: Double): Boolean;
    function GetPerformanceReport: string;
    
    // Test Analysis and Reporting
    function GetTestResults(const SuiteID: string = ''): TArray<TTestResult>;
    function GetTestStatistics: TJSONObject;
    function GetFailedTests: TArray<TTestResult>;
    function GetSlowTests(const ThresholdSeconds: Double = 5.0): TArray<TTestResult>;
    function GetTestCoverage: TJSONObject;
    function GenerateTestReport(const ReportType: string = 'html'): string;
    function ExportTestResults(const FileName: string; Format: string = 'json'): Boolean;
    function ImportTestResults(const FileName: string): Boolean;
    
    // Mock and Stub Utilities
    function CreateMockOllamaAPI: TOllamaAPI;
    function CreateMockTrainingEngine: TTrainingEngine;
    function CreateMockDataProcessor: TDataProcessor;
    function CreateTestData(const DataType: string; const Size: Integer): TJSONObject;
    function CreateTestModel(const ModelName: string): Boolean;
    function SetupTestEnvironment: Boolean;
    function CleanupTestEnvironment: Boolean;
    
    // Debugging and Diagnostics
    function DiagnoseFailedTest(const TestID: string): TJSONObject;
    function GetTestDependencies(const TestName: string): TArray<string>;
    function ValidateTestEnvironment: TArray<string>;
    function GetSystemRequirements: TJSONObject;
    function CheckDependencies: TArray<string>;
    function RepairTestEnvironment: TArray<string>;
    
    // Configuration and Settings
    procedure LoadConfiguration(const ConfigFile: string);
    procedure SaveConfiguration(const ConfigFile: string);
    procedure SetTestTimeout(const TestName: string; TimeoutSeconds: Integer);
    procedure SetRetryCount(const TestName: string; RetryCount: Integer);
    procedure EnableParallelExecution(const SuiteID: string; Enabled: Boolean = True);
    procedure SetTestPriority(const TestName: string; Priority: TTestPriority);
    
    // Utilities
    procedure Clear;
    procedure Reset;
    function GetTestCount: Integer;
    function GetPassedTestCount: Integer;
    function GetFailedTestCount: Integer;
    function GetTestSuccessRate: Double;
    function EstimateTestDuration(const SuiteID: string): TDateTime;
    function IsTestRunning(const TestName: string): Boolean;
    function CancelRunningTests: Boolean;
  end;

implementation

uses
  System.Hash, System.NetEncoding, Winapi.Windows, Winapi.PsAPI;

{ TValidationTests }

constructor TValidationTests.Create(OllamaAPI: TOllamaAPI; TrainingEngine: TTrainingEngine;
  ModelManager: TModelManager; DataProcessor: TDataProcessor;
  DatabaseManager: TDatabaseManager; Logger: TLogger);
begin
  inherited Create;
  
  FOllamaAPI := OllamaAPI;
  FTrainingEngine := TrainingEngine;
  FModelManager := ModelManager;
  FDataProcessor := DataProcessor;
  FDatabaseManager := DatabaseManager;
  FLogger := Logger;
  
  FTestSuites := TDictionary<string, TTestSuite>.Create;
  FTestResults := TList<TTestResult>.Create;
  FValidationRules := TDictionary<string, TValidationRule>.Create;
  FBenchmarks := TDictionary<string, TPerformanceBenchmark>.Create;
  FRunningTests := TDictionary<string, ITask>.Create;
  
  // Default configuration
  FEnabled := True;
  FMaxConcurrentTests := 4;
  FDefaultTimeout := 300; // 5 minutes
  FRetryFailedTests := True;
  FDefaultRetryCount := 3;
  FGenerateReports := True;
  FReportPath := TPath.Combine(TPath.GetDocumentsPath, 'OllamaTrainer', 'TestReports');
  FContinueOnFailure := True;
  FVerboseLogging := False;
  FCollectCoverage := True;
  FCollectPerformanceMetrics := True;
  
  ForceDirectories(FReportPath);
  
  // Initialize built-in validation rules
  InitializeDefaultValidationRules;
  
  // Initialize built-in test suites
  InitializeDefaultTestSuites;
  
  // Initialize built-in benchmarks
  InitializeDefaultBenchmarks;
  
  if Assigned(FLogger) then
    FLogger.Info('ValidationTests system initialized', lcSystem);
end;

destructor TValidationTests.Destroy;
var
  TestResult: TTestResult;
  Rule: TValidationRule;
  Benchmark: TPerformanceBenchmark;
begin
  // Cancel any running tests
  CancelRunningTests;
  
  // Free test results
  for var I := 0 to FTestResults.Count - 1 do
  begin
    TestResult := FTestResults[I];
    if Assigned(TestResult.Metadata) then
      TestResult.Metadata.Free;
    if Assigned(TestResult.TestData) then
      TestResult.TestData.Free;
    if Assigned(TestResult.Environment) then
      TestResult.Environment.Free;
  end;
  
  // Free validation rules
  for var Pair in FValidationRules do
  begin
    Rule := Pair.Value;
    if Assigned(Rule.Parameters) then
      Rule.Parameters.Free;
  end;
  
  FRunningTests.Free;
  FBenchmarks.Free;
  FValidationRules.Free;
  FTestResults.Free;
  FTestSuites.Free;
  
  inherited Destroy;
end;

function TValidationTests.CreateTestSuite(const Name, Description: string; 
  TestTypes: TArray<TTestType>): string;
var
  Suite: TTestSuite;
begin
  Result := GenerateTestID;
  
  Suite.SuiteID := Result;
  Suite.Name := Name;
  Suite.Description := Description;
  Suite.TestTypes := TestTypes;
  SetLength(Suite.Tests, 0);
  Suite.Timeout := FDefaultTimeout;
  Suite.RetryCount := FDefaultRetryCount;
  Suite.Parallel := False;
  SetLength(Suite.Dependencies, 0);
  SetLength(Suite.Tags, 0);
  Suite.Configuration := TJSONObject.Create;
  Suite.CreatedAt := Now;
  Suite.LastRun := 0;
  
  FTestSuites.Add(Result, Suite);
  
  if Assigned(FLogger) then
    FLogger.Info(Format('Test suite created: %s (%s)', [Name, Result]), lcSystem);
end;

function TValidationTests.RunTest(const TestName: string): TTestResult;
var
  TestProc: TProc;
  StartTime: TDateTime;
begin
  // Initialize test result
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := TestName;
  Result.TestType := ttUnit;
  Result.Status := tsRunning;
  Result.Priority := tpMedium;
  Result.Severity := tsMajor;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  CollectTestEnvironment(Result.Environment);
  
  if Assigned(FOnTestStarted) then
    FOnTestStarted(Self, Result);
  
  StartTime := Now;
  
  try
    // Determine which test to run based on name
    if SameText(TestName, 'TestOllamaConnection') then
      Result := TestOllamaConnection
    else if SameText(TestName, 'TestModelLoading') then
      Result := TestModelLoading
    else if SameText(TestName, 'TestDataProcessing') then
      Result := TestDataProcessing
    else if SameText(TestName, 'TestDatabaseConnection') then
      Result := TestDatabaseConnection
    else if SameText(TestName, 'TestTrainingPipeline') then
      Result := TestTrainingPipeline
    else if SameText(TestName, 'TestEndToEndTraining') then
      Result := TestEndToEndTraining
    else if SameText(TestName, 'TestModelManagement') then
      Result := TestModelManagement
    else if SameText(TestName, 'TestDataFlow') then
      Result := TestDataFlow
    else if SameText(TestName, 'TestReportGeneration') then
      Result := TestReportGeneration
    else if SameText(TestName, 'TestUserWorkflow') then
      Result := TestUserWorkflow
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'Test not found: ' + TestName;
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
    if Result.Status = tsRunning then
      Result.Status := tsPassed;
      
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.StackTrace := 'Stack trace not implemented';
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
  
  // Add to results
  FTestResults.Add(Result);
  
  // Log result
  LogTestResult(Result);
  
  if Assigned(FOnTestCompleted) then
    FOnTestCompleted(Self, Result);
end;

function TValidationTests.TestOllamaConnection: TTestResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := 'TestOllamaConnection';
  Result.TestType := ttIntegration;
  Result.Status := tsRunning;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  try
    // Test Ollama API connection
    if Assigned(FOllamaAPI) then
    begin
      var ConnectionTest := FOllamaAPI.TestConnection;
      
      if ConnectionTest then
      begin
        Result.Status := tsPassed;
        Result.ActualResult := 'Connection successful';
        Result.TestData.AddPair('connection_status', 'success');
        
        // Test getting server info
        var ServerInfo := FOllamaAPI.GetServerInfo;
        if Assigned(ServerInfo) then
        begin
          Result.TestData.AddPair('server_info', ServerInfo.Clone as TJSONObject);
          ServerInfo.Free;
        end;
      end
      else
      begin
        Result.Status := tsFailed;
        Result.ErrorMessage := 'Failed to connect to Ollama API';
        Result.TestData.AddPair('connection_status', 'failed');
      end;
    end
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'Ollama API instance not available';
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
end;

function TValidationTests.TestModelLoading: TTestResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := 'TestModelLoading';
  Result.TestType := ttIntegration;
  Result.Status := tsRunning;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  try
    if Assigned(FModelManager) then
    begin
      var ModelList := TStringList.Create;
      try
        var Success := FModelManager.GetModelList(ModelList);
        
        if Success then
        begin
          Result.Status := tsPassed;
          Result.ActualResult := Format('Found %d models', [ModelList.Count]);
          Result.TestData.AddPair('model_count', TJSONNumber.Create(ModelList.Count));
          
          var ModelsArray := TJSONArray.Create;
          for var ModelName in ModelList do
            ModelsArray.AddElement(TJSONString.Create(ModelName));
          Result.TestData.AddPair('models', ModelsArray);
          
          // Test loading model info for first model
          if ModelList.Count > 0 then
          begin
            var ModelInfo: TModelInfo;
            if FModelManager.GetModelInfo(ModelList[0], ModelInfo) then
            begin
              Result.TestData.AddPair('first_model_size', TJSONNumber.Create(ModelInfo.Size));
              Result.TestData.AddPair('first_model_name', ModelInfo.Name);
            end;
          end;
        end
        else
        begin
          Result.Status := tsFailed;
          Result.ErrorMessage := 'Failed to get model list';
        end;
        
      finally
        ModelList.Free;
      end;
    end
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'ModelManager instance not available';
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
end;

function TValidationTests.TestDataProcessing: TTestResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := 'TestDataProcessing';
  Result.TestType := ttUnit;
  Result.Status := tsRunning;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  try
    if Assigned(FDataProcessor) then
    begin
      // Test basic data processing functionality
      var TestDataPath := TPath.Combine(TPath.GetTempPath, 'test_data');
      ForceDirectories(TestDataPath);
      
      // Create a test file
      var TestFile := TPath.Combine(TestDataPath, 'test.txt');
      TFile.WriteAllText(TestFile, 'This is test data for processing.');
      
      try
        var FileCount := FDataProcessor.CountTrainingFiles(TestDataPath, False);
        
        if FileCount > 0 then
        begin
          Result.Status := tsPassed;
          Result.ActualResult := Format('Processed %d files', [FileCount]);
          Result.TestData.AddPair('file_count', TJSONNumber.Create(FileCount));
          Result.TestData.AddPair('test_path', TestDataPath);
        end
        else
        begin
          Result.Status := tsFailed;
          Result.ErrorMessage := 'No files found for processing';
        end;
        
      finally
        // Cleanup
        if FileExists(TestFile) then
          DeleteFile(TestFile);
        RemoveDir(TestDataPath);
      end;
    end
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'DataProcessor instance not available';
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
end;

function TValidationTests.TestDatabaseConnection: TTestResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := 'TestDatabaseConnection';
  Result.TestType := ttIntegration;
  Result.Status := tsRunning;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  try
    if Assigned(FDatabaseManager) then
    begin
      // Test basic database operations
      var TestQuery := 'SELECT COUNT(*) as count FROM sqlite_master WHERE type=''table''';
      var QueryResult := FDatabaseManager.ExecuteQuery(TestQuery, qrfJSON);
      
      if VarIsType(QueryResult, varUnknown) then
      begin
        var JSONResult := TJSONArray(TVarData(QueryResult).VUnknown);
        if Assigned(JSONResult) and (JSONResult.Count > 0) then
        begin
          Result.Status := tsPassed;
          Result.ActualResult := 'Database connection successful';
          Result.TestData.AddPair('table_count', JSONResult.Items[0]);
        end
        else
        begin
          Result.Status := tsFailed;
          Result.ErrorMessage := 'Empty result from database query';
        end;
      end
      else
      begin
        Result.Status := tsFailed;
        Result.ErrorMessage := 'Invalid result type from database query';
      end;
    end
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'DatabaseManager instance not available';
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
end;

function TValidationTests.TestTrainingPipeline: TTestResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TestID := GenerateTestID;
  Result.TestName := 'TestTrainingPipeline';
  Result.TestType := ttIntegration;
  Result.Status := tsRunning;
  Result.StartTime := Now;
  Result.Metadata := TJSONObject.Create;
  Result.TestData := TJSONObject.Create;
  Result.Environment := TJSONObject.Create;
  
  try
    if Assigned(FTrainingEngine) then
    begin
      // Test training engine state
      Result.TestData.AddPair('engine_state', GetEnumName(TypeInfo(TTrainingState), Ord(FTrainingEngine.State)));
      Result.TestData.AddPair('current_epoch', TJSONNumber.Create(FTrainingEngine.CurrentEpoch));
      Result.TestData.AddPair('total_epochs', TJSONNumber.Create(FTrainingEngine.TotalEpochs));
      
      Result.Status := tsPassed;
      Result.ActualResult := 'Training pipeline accessible';
    end
    else
    begin
      Result.Status := tsError;
      Result.ErrorMessage := 'TrainingEngine instance not available';
    end;
    
    Result.EndTime := Now;
    Result.Duration := Result.EndTime - Result.StartTime;
    
  except
    on E: Exception do
    begin
      Result.Status := tsError;
      Result.ErrorMessage := E.Message;
      Result.EndTime := Now;
      Result.Duration := Result.EndTime - Result.StartTime;
    end;
  end;
end;

function TValidationTests.RunSystemValidationTests: TArray<TTestResult>;
var
  TestNames: TArray<string>;
  I: Integer;
begin
  TestNames := ['TestOllamaConnection', 'TestModelLoading', 'TestDataProcessing', 
                'TestDatabaseConnection', 'TestTrainingPipeline'];
  
  SetLength(Result, Length(TestNames));
  
  for I := 0 to High(TestNames) do
    Result[I] := RunTest(TestNames[I]);
end;

function TValidationTests.ValidateData(const Data: TJSONObject; 
  const Rules: TArray<TValidationRule>): TValidationResult;
var
  I: Integer;
  Rule: TValidationRule;
  ValidationPassed: Boolean;
  ErrorList: TList<string>;
  WarningList: TList<string>;
  PassedRules: TList<string>;
  FailedRules: TList<string>;
  StartTime: TDateTime;
begin
  StartTime := Now;
  ErrorList := TList<string>.Create;
  WarningList := TList<string>.Create;
  PassedRules := TList<string>.Create;
  FailedRules := TList<string>.Create;
  
  try
    Result.Details := TJSONObject.Create;
    Result.ValidatedFields := 0;
    
    for I := 0 to High(Rules) do
    begin
      Rule := Rules[I];
      
      if not Rule.Enabled then
        Continue;
      
      Inc(Result.ValidatedFields);
      ValidationPassed := True;
      
      // Apply validation rule based on type
      case Rule.RuleType of
        'required':
        begin
          if not Data.GetValue(Rule.Name) then
          begin
            ValidationPassed := False;
            if Rule.Severity in [tsCritical, tsBlocker] then
              ErrorList.Add(Rule.ErrorMessage)
            else
              WarningList.Add(Rule.WarningMessage);
          end;
        end;
        
        'format':
        begin
          var Value := Data.GetValue<string>(Rule.Name, '');
          if (Value <> '') and not ValidateFormat(Value, Rule.Condition) then
          begin
            ValidationPassed := False;
            if Rule.Severity in [tsCritical, tsBlocker] then
              ErrorList.Add(Rule.ErrorMessage)
            else
              WarningList.Add(Rule.WarningMessage);
          end;
        end;
        
        'range':
        begin
          var Value := Data.GetValue(Rule.Name);
          if not VarIsNull(Value) then
          begin
            var RangeParams := Rule.Parameters;
            var MinValue := RangeParams.GetValue('min');
            var MaxValue := RangeParams.GetValue('max');
            
            if not ValidateRange(Value, MinValue, MaxValue) then
            begin
              ValidationPassed := False;
              if Rule.Severity in [tsCritical, tsBlocker] then
                ErrorList.Add(Rule.ErrorMessage)
              else
                WarningList.Add(Rule.WarningMessage);
            end;
          end;
        end;
        
        'custom':
        begin
          var Value := Data.GetValue(Rule.Name);
          if Assigned(Rule.ValidationFunction) and not Rule.ValidationFunction(Value) then
          begin
            ValidationPassed := False;
            if Rule.Severity in [tsCritical, tsBlocker] then
              ErrorList.Add(Rule.ErrorMessage)
            else
              WarningList.Add(Rule.WarningMessage);
          end;
        end;
      end;
      
      if ValidationPassed then
        PassedRules.Add(Rule.RuleID)
      else
        FailedRules.Add(Rule.RuleID);
    end;
    
    // Set results
    Result.IsValid := ErrorList.Count = 0;
    
    SetLength(Result.Errors, ErrorList.Count);
    for I := 0 to ErrorList.Count - 1 do
      Result.Errors[I] := ErrorList[I];
    
    SetLength(Result.Warnings, WarningList.Count);
    for I := 0 to WarningList.Count - 1 do
      Result.Warnings[I] := WarningList[I];
    
    SetLength(Result.PassedRules, PassedRules.Count);
    for I := 0 to PassedRules.Count - 1 do
      Result.PassedRules[I] := PassedRules[I];
    
    SetLength(Result.FailedRules, FailedRules.Count);
    for I := 0 to FailedRules.Count - 1 do
      Result.FailedRules[I] := FailedRules[I];
    
    Result.ValidationTime := Now - StartTime;
    
    // Add summary to details
    Result.Details.AddPair('total_rules', TJSONNumber.Create(Length(Rules)));
    Result.Details.AddPair('passed_rules', TJSONNumber.Create(PassedRules.Count));
    Result.Details.AddPair('failed_rules', TJSONNumber.Create(FailedRules.Count));
    Result.Details.AddPair('error_count', TJSONNumber.Create(ErrorList.Count));
    Result.Details.AddPair('warning_count', TJSONNumber.Create(WarningList.Count));
    Result.Details.AddPair('validation_time_ms', TJSONNumber.Create(Result.ValidationTime * SecsPerDay * 1000));
    
  finally
    FailedRules.Free;
    PassedRules.Free;
    WarningList.Free;
    ErrorList.Free;
  end;
  
  if Assigned(FOnValidationCompleted) then
    FOnValidationCompleted(Self, Result);
end;

// Helper methods implementation

function TValidationTests.GenerateTestID: string;
begin
  Result := 'test_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + 
    IntToStr(Random(1000)).PadLeft(3, '0');
end;

procedure TValidationTests.LogTestResult(const TestResult: TTestResult);
var
  LogLevel: TLogLevel;
  LogMessage: string;
begin
  if not Assigned(FLogger) then
    Exit;
  
  case TestResult.Status of
    tsPassed: LogLevel := llInfo;
    tsFailed: LogLevel := llWarning;
    tsError: LogLevel := llError;
  else
    LogLevel := llInfo;
  end;
  
  LogMessage := Format('Test %s: %s (Duration: %s)', 
    [TestResult.TestName, 
     GetEnumName(TypeInfo(TTestStatus), Ord(TestResult.Status)),
     FormatDateTime('hh:nn:ss.zzz', TestResult.Duration)]);
     
  if TestResult.ErrorMessage <> '' then
    LogMessage := LogMessage + ' - ' + TestResult.ErrorMessage;
  
  FLogger.Log(LogLevel, LogMessage, lcGeneral);
end;

procedure TValidationTests.CollectTestEnvironment(var Environment: TJSONObject);
begin
  if not Assigned(Environment) then
    Environment := TJSONObject.Create;
  
  Environment.AddPair('timestamp', DateToISO8601(Now));
  Environment.AddPair('os_version', TOSVersion.ToString);
  Environment.AddPair('machine_name', GetEnvironmentVariable('COMPUTERNAME'));
  Environment.AddPair('user_name', GetEnvironmentVariable('USERNAME'));
  Environment.AddPair('temp_path', TPath.GetTempPath);
  Environment.AddPair('app_version', '2.0.0'); // Should come from application version
end;

function TValidationTests.ValidateFormat(const Value: string; const Pattern: string): Boolean;
begin
  Result := TRegEx.IsMatch(Value, Pattern);
end;

function TValidationTests.ValidateRange(const Value: Variant; const Min, Max: Variant): Boolean;
begin
  Result := True;
  
  if not VarIsNull(Min) and (Value < Min) then
    Result := False;
    
  if not VarIsNull(Max) and (Value > Max) then
    Result := False;
end;

function TValidationTests.ValidateRequired(const Value: Variant): Boolean;
begin
  Result := not VarIsNull(Value) and (VarToStr(Value) <> '');
end;

// Assertion methods
procedure TValidationTests.AssertEquals(const Expected, Actual: Variant; const Message: string);
begin
  if Expected <> Actual then
    raise Exception.Create(Format('Assertion failed: Expected <%s> but was <%s>. %s', 
      [VarToStr(Expected), VarToStr(Actual), Message]));
end;

procedure TValidationTests.AssertTrue(const Condition: Boolean; const Message: string);
begin
  if not Condition then
    raise Exception.Create('Assertion failed: Expected <True> but was <False>. ' + Message);
end;

procedure TValidationTests.AssertFalse(const Condition: Boolean; const Message: string);
begin
  if Condition then
    raise Exception.Create('Assertion failed: Expected <False> but was <True>. ' + Message);
end;

procedure TValidationTests.AssertNotNull(const Value: Variant; const Message: string);
begin
  if VarIsNull(Value) then
    raise Exception.Create('Assertion failed: Expected non-null value. ' + Message);
end;

// Placeholder implementations for remaining methods
procedure TValidationTests.InitializeDefaultValidationRules; begin end;
procedure TValidationTests.InitializeDefaultTestSuites; begin end;
procedure TValidationTests.InitializeDefaultBenchmarks; begin end;
function TValidationTests.TestEndToEndTraining: TTestResult; begin FillChar(Result, SizeOf(Result), 0); end;
function TValidationTests.TestModelManagement: TTestResult; begin FillChar(Result, SizeOf(Result), 0); end;
function TValidationTests.TestDataFlow: TTestResult; begin FillChar(Result, SizeOf(Result), 0); end;
function TValidationTests.TestReportGeneration: TTestResult; begin FillChar(Result, SizeOf(Result), 0); end;
function TValidationTests.TestUserWorkflow: TTestResult; begin FillChar(Result, SizeOf(Result), 0); end;
procedure TValidationTests.UpdateTestStatistics(const TestResult: TTestResult); begin end;
function TValidationTests.CancelRunningTests: Boolean; begin Result := True; end;

// More placeholder implementations
function TValidationTests.GetTestCount: Integer; begin Result := FTestResults.Count; end;
function TValidationTests.GetPassedTestCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  for var TestResult in FTestResults do
    if TestResult.Status = tsPassed then
      Inc(Count);
  Result := Count;
end;

function TValidationTests.GetFailedTestCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  for var TestResult in FTestResults do
    if TestResult.Status in [tsFailed, tsError] then
      Inc(Count);
  Result := Count;
end;

function TValidationTests.GetTestSuccessRate: Double;
var
  Total: Integer;
begin
  Total := GetTestCount;
  if Total > 0 then
    Result := (GetPassedTestCount / Total) * 100
  else
    Result := 0;
end;

// Additional basic implementations
procedure TValidationTests.AssertNotEquals(const Expected, Actual: Variant; const Message: string); begin if Expected = Actual then raise Exception.Create('Values should not be equal. ' + Message); end;
procedure TValidationTests.AssertNull(const Value: Variant; const Message: string); begin if not VarIsNull(Value) then raise Exception.Create('Expected null value. ' + Message); end;
procedure TValidationTests.AssertGreaterThan(const Expected, Actual: Variant; const Message: string); begin if Actual <= Expected then raise Exception.Create('Value should be greater. ' + Message); end;
procedure TValidationTests.AssertLessThan(const Expected, Actual: Variant; const Message: string); begin if Actual >= Expected then raise Exception.Create('Value should be less. ' + Message); end;
procedure TValidationTests.AssertInRange(const Value, Min, Max: Variant; const Message: string); begin if (Value < Min) or (Value > Max) then raise Exception.Create('Value not in range. ' + Message); end;
procedure TValidationTests.AssertContains(const Container, Item: string; const Message: string); begin if not ContainsStr(Container, Item) then raise Exception.Create('String not found. ' + Message); end;
procedure TValidationTests.AssertMatches(const Pattern, Text: string; const Message: string); begin if not TRegEx.IsMatch(Text, Pattern) then raise Exception.Create('Pattern not matched. ' + Message); end;

end.