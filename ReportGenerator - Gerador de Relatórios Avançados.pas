unit ReportGenerator;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.IOUtils, System.DateUtils, System.Math, System.RegularExpressions,
  System.Threading, Vcl.Graphics, Vcl.Charts, TrainingEngine, ModelManager, Logger;

type
  TReportType = (rtTrainingProgress, rtModelComparison, rtPerformanceAnalysis, 
    rtDataQuality, rtSystemHealth, rtUsageStatistics, rtErrorAnalysis, rtCustom);
    
  TReportFormat = (rfHTML, rfPDF, rfMarkdown, rfJSON, rfCSV, rfExcel, rfPowerPoint);
  
  TChartType = (ctLine, ctBar, ctPie, ctArea, ctScatter, ctHistogram, ctHeatmap);
  
  TReportSection = record
    Title: string;
    Content: string;
    SectionType: string; // text, chart, table, image
    Data: TJSONObject;
    Order: Integer;
    Visible: Boolean;
    ChartType: TChartType;
    ChartData: TArray<TArray<Variant>>;
  end;
  
  TReportTemplate = record
    Name: string;
    Description: string;
    ReportType: TReportType;
    Sections: TArray<TReportSection>;
    StyleSheet: string;
    Parameters: TJSONObject;
    Author: string;
    Version: string;
    CreatedDate: TDateTime;
  end;
  
  TReportConfiguration = record
    Title: string;
    Subtitle: string;
    Author: string;
    Company: string;
    Logo: string;
    Theme: string;
    IncludeTOC: Boolean;
    IncludeCharts: Boolean;
    IncludeStatistics: Boolean;
    IncludeRecommendations: Boolean;
    DateRange: record
      StartDate: TDateTime;
      EndDate: TDateTime;
    end;
    Filters: TJSONObject;
    CustomFields: TJSONObject;
  end;
  
  TDataSource = record
    Name: string;
    SourceType: string; // database, api, file, memory
    ConnectionString: string;
    Query: string;
    Data: TJSONArray;
    LastUpdated: TDateTime;
  end;
  
  TReportMetric = record
    Name: string;
    Value: Variant;
    Unit_: string;
    Trend: string; // up, down, stable
    TrendValue: Double;
    Benchmark: Variant;
    Status: string; // good, warning, critical
    Description: string;
    Color: TColor;
  end;
  
  TReportGenerationEvent = procedure(Sender: TObject; Progress: Integer; 
    const Status: string) of object;
  TReportCompleteEvent = procedure(Sender: TObject; Success: Boolean; 
    const FileName: string; const Message: string) of object;
  TDataProcessingEvent = procedure(Sender: TObject; const DataSource: string; 
    RecordsProcessed: Integer) of object;
    
  TReportGenerator = class
  private
    FTemplates: TDictionary<string, TReportTemplate>;
    FDataSources: TDictionary<string, TDataSource>;
    FConfiguration: TReportConfiguration;
    FGeneratedReports: TStringList;
    FOutputPath: string;
    FDefaultTemplate: string;
    FTrainingEngine: TTrainingEngine;
    FModelManager: TModelManager;
    FLogger: TLogger;
    
    // Cache e performance
    FDataCache: TDictionary<string, TJSONArray>;
    FCacheTimeout: Integer;
    FLastCacheUpdate: TDateTime;
    
    // Threading
    FGenerationTask: ITask;
    FAsyncGeneration: Boolean;
    
    // Eventos
    FOnProgress: TReportGenerationEvent;
    FOnComplete: TReportCompleteEvent;
    FOnDataProcessing: TDataProcessingEvent;
    
    // Configurações
    FIncludeRawData: Boolean;
    FCompressionEnabled: Boolean;
    FEncryptionEnabled: Boolean;
    FWatermarkEnabled: Boolean;
    FWatermarkText: string;
    
    function LoadTemplate(const TemplateName: string): TReportTemplate;
    function ProcessDataSource(const DataSource: TDataSource): TJSONArray;
    function GenerateSection(const Section: TReportSection; const Data: TJSONArray): string;
    function CreateChart(const ChartType: TChartType; const Data: TArray<TArray<Variant>>; 
      const Title: string): string;
    function GenerateHTMLReport(const Template: TReportTemplate; const Data: TJSONObject): string;
    function GenerateMarkdownReport(const Template: TReportTemplate; const Data: TJSONObject): string;
    function GenerateJSONReport(const Template: TReportTemplate; const Data: TJSONObject): string;
    function GenerateCSVReport(const Template: TReportTemplate; const Data: TJSONObject): string;
    function ConvertToPDF(const HTMLContent: string; const OutputFile: string): Boolean;
    function ConvertToExcel(const Data: TJSONObject; const OutputFile: string): Boolean;
    function ApplyTheme(const Content: string; const Theme: string): string;
    function GetHTMLTemplate: string;
    function GetCSSStyles(const Theme: string): string;
    function CollectTrainingData: TJSONObject;
    function CollectModelData: TJSONObject;
    function CollectPerformanceData: TJSONObject;
    function CollectSystemData: TJSONObject;
    function CollectUsageData: TJSONObject;
    function CalculateMetrics(const Data: TJSONObject): TArray<TReportMetric>;
    function GenerateRecommendations(const Metrics: TArray<TReportMetric>): TStringList;
    function FormatMetric(const Metric: TReportMetric): string;
    function CreateTableOfContents(const Sections: TArray<TReportSection>): string;
    function ProcessChartData(const RawData: TJSONArray; const ChartType: TChartType): TArray<TArray<Variant>>;
    function ValidateReportData(const Data: TJSONObject): Boolean;
    function OptimizeImages(const Content: string): string;
    function AddWatermark(const Content: string): string;
    procedure UpdateProgress(Progress: Integer; const Status: string);
    procedure LogReportGeneration(const ReportType: string; Success: Boolean; const Details: string);
    
  public
    constructor Create(TrainingEngine: TTrainingEngine; ModelManager: TModelManager; Logger: TLogger);
    destructor Destroy; override;
    
    // Propriedades
    property OutputPath: string read FOutputPath write FOutputPath;
    property DefaultTemplate: string read FDefaultTemplate write FDefaultTemplate;
    property AsyncGeneration: Boolean read FAsyncGeneration write FAsyncGeneration;
    property IncludeRawData: Boolean read FIncludeRawData write FIncludeRawData;
    property CompressionEnabled: Boolean read FCompressionEnabled write FCompressionEnabled;
    property WatermarkEnabled: Boolean read FWatermarkEnabled write FWatermarkEnabled;
    property WatermarkText: string read FWatermarkText write FWatermarkText;
    property Configuration: TReportConfiguration read FConfiguration write FConfiguration;
    
    // Eventos
    property OnProgress: TReportGenerationEvent read FOnProgress write FOnProgress;
    property OnComplete: TReportCompleteEvent read FOnComplete write FOnComplete;
    property OnDataProcessing: TDataProcessingEvent read FOnDataProcessing write FOnDataProcessing;
    
    // Geração de relatórios principais
    function GenerateTrainingReport(const ModelName: string; Format: TReportFormat = rfHTML): string;
    function GenerateModelComparisonReport(const ModelNames: TArray<string>; Format: TReportFormat = rfHTML): string;
    function GeneratePerformanceReport(StartDate, EndDate: TDateTime; Format: TReportFormat = rfHTML): string;
    function GenerateSystemHealthReport(Format: TReportFormat = rfHTML): string;
    function GenerateUsageStatisticsReport(StartDate, EndDate: TDateTime; Format: TReportFormat = rfHTML): string;
    function GenerateErrorAnalysisReport(StartDate, EndDate: TDateTime; Format: TReportFormat = rfHTML): string;
    function GenerateCustomReport(const TemplateName: string; const Parameters: TJSONObject; Format: TReportFormat = rfHTML): string;
    
    // Geração assíncrona
    function GenerateReportAsync(ReportType: TReportType; const Parameters: TJSONObject; Format: TReportFormat = rfHTML): ITask;
    function GenerateBatchReports(const Requests: TArray<TJSONObject>): TArray<string>;
    
    // Gerenciamento de templates
    function LoadTemplateFromFile(const FileName: string): Boolean;
    function SaveTemplateToFile(const Template: TReportTemplate; const FileName: string): Boolean;
    function CreateTemplate(const Name: string; ReportType: TReportType): TReportTemplate;
    function UpdateTemplate(const Name: string; const Template: TReportTemplate): Boolean;
    function DeleteTemplate(const Name: string): Boolean;
    function GetAvailableTemplates: TArray<string>;
    function CloneTemplate(const SourceName, TargetName: string): Boolean;
    function ValidateTemplate(const Template: TReportTemplate): TArray<string>;
    
    // Gerenciamento de fontes de dados
    function AddDataSource(const DataSource: TDataSource): Boolean;
    function RemoveDataSource(const Name: string): Boolean;
    function UpdateDataSource(const Name: string; const DataSource: TDataSource): Boolean;
    function RefreshDataSource(const Name: string): Boolean;
    function GetDataSources: TArray<string>;
    function TestDataConnection(const DataSource: TDataSource): Boolean;
    
    // Análise e insights
    function AnalyzeTrainingTrends(const ModelName: string; Days: Integer = 30): TJSONObject;
    function AnalyzeModelPerformance(const ModelNames: TArray<string>): TJSONObject;
    function AnalyzeSystemUsage(Days: Integer = 7): TJSONObject;
    function GenerateInsights(const Data: TJSONObject): TStringList;
    function CalculateTrends(const Data: TJSONArray; const MetricName: string): TJSONObject;
    function PredictFutureMetrics(const HistoricalData: TJSONArray; Days: Integer = 7): TJSONArray;
    
    // Exportação e distribuição
    function ExportReport(const ReportContent: string; Format: TReportFormat; const FileName: string): Boolean;
    function EmailReport(const ReportContent: string; const Recipients: TArray<string>; const Subject: string): Boolean;
    function UploadReport(const ReportContent: string; const UploadURL: string): Boolean;
    function ScheduleReport(ReportType: TReportType; const Schedule: string; const Parameters: TJSONObject): Boolean;
    function CreateReportArchive(const ReportFiles: TArray<string>; const ArchiveName: string): Boolean;
    
    // Configuração e personalização
    procedure SetConfiguration(const Config: TReportConfiguration);
    procedure LoadConfiguration(const ConfigFile: string);
    procedure SaveConfiguration(const ConfigFile: string);
    procedure SetTheme(const ThemeName: string);
    procedure AddCustomChart(const ChartName: string; const ChartDefinition: TJSONObject);
    procedure SetReportHeader(const HeaderTemplate: string);
    procedure SetReportFooter(const FooterTemplate: string);
    
    // Dashboard e relatórios em tempo real
    function GenerateDashboard(const Widgets: TArray<string>): string;
    function GenerateRealTimeReport(ReportType: TReportType; RefreshInterval: Integer): string;
    function CreateWidget(const WidgetType, DataSource: string; const Configuration: TJSONObject): string;
    function UpdateDashboard(const DashboardID: string; const NewData: TJSONObject): Boolean;
    
    // Utilitários
    function GetReportHistory: TStringList;
    function CleanupOldReports(OlderThanDays: Integer): Integer;
    function ValidateReportFormat(Format: TReportFormat): Boolean;
    function EstimateGenerationTime(ReportType: TReportType; const DataSize: Integer): TDateTime;
    function GetReportSize(const ReportFile: string): Int64;
    function CompressReport(const ReportFile: string): Boolean;
    function GetReportMetadata(const ReportFile: string): TJSONObject;
    
    // Relatórios especializados
    function GenerateTrainingProgressChart(const ModelName: string): string;
    function GenerateModelAccuracyComparison(const ModelNames: TArray<string>): string;
    function GenerateDataQualityReport(const DatasetPath: string): string;
    function GenerateResourceUsageReport: string;
    function GenerateSecurityAuditReport: string;
    function GenerateComplianceReport(const Standards: TArray<string>): string;
  end;

implementation

uses
  System.Variants, System.NetEncoding, System.Hash, System.Zip,
  Winapi.Windows, Winapi.ShellAPI;

{ TReportGenerator }

constructor TReportGenerator.Create(TrainingEngine: TTrainingEngine; ModelManager: TModelManager; Logger: TLogger);
begin
  inherited Create;
  
  FTrainingEngine := TrainingEngine;
  FModelManager := ModelManager;
  FLogger := Logger;
  
  FTemplates := TDictionary<string, TReportTemplate>.Create;
  FDataSources := TDictionary<string, TDataSource>.Create;
  FDataCache := TDictionary<string, TJSONArray>.Create;
  FGeneratedReports := TStringList.Create;
  
  // Configurações padrão
  FOutputPath := TPath.Combine(TPath.GetDocumentsPath, 'OllamaTrainer', 'Reports');
  FDefaultTemplate := 'standard';
  FAsyncGeneration := True;
  FIncludeRawData := False;
  FCompressionEnabled := True;
  FEncryptionEnabled := False;
  FWatermarkEnabled := True;
  FWatermarkText := 'Generated by OllamaTrainer';
  FCacheTimeout := 300; // 5 minutos
  
  // Garantir que diretório existe
  ForceDirectories(FOutputPath);
  
  // Inicializar templates padrão
  InitializeDefaultTemplates;
  
  // Configuração padrão
  InitializeDefaultConfiguration;
  
  if Assigned(FLogger) then
    FLogger.Info('ReportGenerator inicializado', lcSystem);
end;

destructor TReportGenerator.Destroy;
var
  Template: TReportTemplate;
  DataSource: TDataSource;
begin
  // Aguardar conclusão de tarefas assíncronas
  if Assigned(FGenerationTask) then
    FGenerationTask.Wait;
  
  // Liberar recursos dos templates
  for var Pair in FTemplates do
  begin
    Template := Pair.Value;
    if Assigned(Template.Parameters) then
      Template.Parameters.Free;
  end;
  
  // Liberar recursos das fontes de dados
  for var Pair in FDataSources do
  begin
    DataSource := Pair.Value;
    if Assigned(DataSource.Data) then
      DataSource.Data.Free;
  end;
  
  // Liberar cache de dados
  for var Pair in FDataCache do
    Pair.Value.Free;
  
  FDataCache.Free;
  FDataSources.Free;
  FTemplates.Free;
  FGeneratedReports.Free;
  
  inherited Destroy;
end;

function TReportGenerator.GenerateTrainingReport(const ModelName: string; Format: TReportFormat): string;
var
  Template: TReportTemplate;
  Data: TJSONObject;
  ReportContent: string;
  FileName: string;
begin
  Result := '';
  
  try
    UpdateProgress(10, 'Carregando template...');
    Template := LoadTemplate('training_report');
    
    UpdateProgress(30, 'Coletando dados de treinamento...');
    Data := CollectTrainingData;
    Data.AddPair('model_name', ModelName);
    
    UpdateProgress(50, 'Processando dados...');
    // Adicionar dados específicos do modelo
    var ModelData := FTrainingEngine.GetTrainingReport;
    Data.AddPair('training_report', ModelData);
    
    UpdateProgress(70, 'Gerando relatório...');
    case Format of
      rfHTML: ReportContent := GenerateHTMLReport(Template, Data);
      rfMarkdown: ReportContent := GenerateMarkdownReport(Template, Data);
      rfJSON: ReportContent := GenerateJSONReport(Template, Data);
      rfCSV: ReportContent := GenerateCSVReport(Template, Data);
    else
      ReportContent := GenerateHTMLReport(Template, Data);
    end;
    
    UpdateProgress(90, 'Salvando arquivo...');
    FileName := TPath.Combine(FOutputPath, 
      Format('training_report_%s_%s.%s', [ModelName, FormatDateTime('yyyymmdd_hhnnss', Now), 
        GetFormatExtension(Format)]));
    
    TFile.WriteAllText(FileName, ReportContent, TEncoding.UTF8);
    
    // Aplicar pós-processamento
    if FCompressionEnabled and (Format = rfHTML) then
      CompressReport(FileName);
    
    if FWatermarkEnabled then
      AddWatermark(ReportContent);
    
    UpdateProgress(100, 'Relatório gerado com sucesso');
    
    FGeneratedReports.Add(FileName);
    Result := FileName;
    
    LogReportGeneration('Training Report', True, 'Modelo: ' + ModelName);
    
    if Assigned(FOnComplete) then
      FOnComplete(Self, True, FileName, 'Relatório de treinamento gerado com sucesso');
      
  except
    on E: Exception do
    begin
      LogReportGeneration('Training Report', False, E.Message);
      
      if Assigned(FOnComplete) then
        FOnComplete(Self, False, '', 'Erro ao gerar relatório: ' + E.Message);
        
      if Assigned(FLogger) then
        FLogger.Error('Erro ao gerar relatório de treinamento: ' + E.Message, lcGeneral);
    end;
  end;
end;

function TReportGenerator.GenerateModelComparisonReport(const ModelNames: TArray<string>; Format: TReportFormat): string;
var
  Template: TReportTemplate;
  Data: TJSONObject;
  ModelsData: TJSONArray;
  ModelInfo: TModelInfo;
  ModelObj: TJSONObject;
  I: Integer;
  ReportContent: string;
  FileName: string;
begin
  Result := '';
  
  try
    UpdateProgress(10, 'Preparando comparação de modelos...');
    Template := LoadTemplate('model_comparison');
    
    UpdateProgress(30, 'Coletando dados dos modelos...');
    Data := TJSONObject.Create;
    ModelsData := TJSONArray.Create;
    
    for I := 0 to High(ModelNames) do
    begin
      if FModelManager.GetModelInfo(ModelNames[I], ModelInfo) then
      begin
        ModelObj := TJSONObject.Create;
        ModelObj.AddPair('name', ModelInfo.Name);
        ModelObj.AddPair('size', TJSONNumber.Create(ModelInfo.Size));
        ModelObj.AddPair('parameters', TJSONNumber.Create(ModelInfo.Parameters));
        ModelObj.AddPair('accuracy', TJSONNumber.Create(ModelInfo.Accuracy));
        ModelObj.AddPair('created', DateToISO8601(ModelInfo.Created));
        ModelObj.AddPair('description', ModelInfo.Description);
        
        ModelsData.AddElement(ModelObj);
      end;
      
      UpdateProgress(30 + (I * 40) div Length(ModelNames), 
        Format('Processando modelo %d de %d', [I + 1, Length(ModelNames)]));
    end;
    
    Data.AddPair('models', ModelsData);
    Data.AddPair('comparison_date', DateToISO8601(Now));
    Data.AddPair('total_models', TJSONNumber.Create(Length(ModelNames)));
    
    UpdateProgress(70, 'Gerando relatório de comparação...');
    
    // Calcular métricas de comparação
    var Metrics := CalculateComparisonMetrics(ModelsData);
    Data.AddPair('metrics', Metrics);
    
    case Format of
      rfHTML: ReportContent := GenerateHTMLReport(Template, Data);
      rfMarkdown: ReportContent := GenerateMarkdownReport(Template, Data);
      rfJSON: ReportContent := GenerateJSONReport(Template, Data);
      rfCSV: ReportContent := GenerateCSVReport(Template, Data);
    else
      ReportContent := GenerateHTMLReport(Template, Data);
    end;
    
    UpdateProgress(90, 'Salvando relatório...');
    FileName := TPath.Combine(FOutputPath, 
      Format('model_comparison_%s.%s', [FormatDateTime('yyyymmdd_hhnnss', Now), 
        GetFormatExtension(Format)]));
    
    TFile.WriteAllText(FileName, ReportContent, TEncoding.UTF8);
    
    UpdateProgress(100, 'Relatório de comparação concluído');
    
    FGeneratedReports.Add(FileName);
    Result := FileName;
    
    LogReportGeneration('Model Comparison', True, 
      Format('Modelos comparados: %d', [Length(ModelNames)]));
    
    if Assigned(FOnComplete) then
      FOnComplete(Self, True, FileName, 'Relatório de comparação gerado com sucesso');
      
  except
    on E: Exception do
    begin
      LogReportGeneration('Model Comparison', False, E.Message);
      
      if Assigned(FOnComplete) then
        FOnComplete(Self, False, '', 'Erro ao gerar relatório: ' + E.Message);
    end;
  end;
end;

function TReportGenerator.GenerateHTMLReport(const Template: TReportTemplate; const Data: TJSONObject): string;
var
  HTMLBuilder: TStringBuilder;
  Section: TReportSection;
  SectionContent: string;
  I: Integer;
begin
  HTMLBuilder := TStringBuilder.Create;
  try
    // Cabeçalho HTML
    HTMLBuilder.AppendLine('<!DOCTYPE html>');
    HTMLBuilder.AppendLine('<html lang="pt-BR">');
    HTMLBuilder.AppendLine('<head>');
    HTMLBuilder.AppendLine('    <meta charset="UTF-8">');
    HTMLBuilder.AppendLine('    <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    HTMLBuilder.AppendLine('    <title>' + FConfiguration.Title + '</title>');
    HTMLBuilder.AppendLine('    <style>');
    HTMLBuilder.AppendLine(GetCSSStyles(FConfiguration.Theme));
    HTMLBuilder.AppendLine('    </style>');
    HTMLBuilder.AppendLine('</head>');
    HTMLBuilder.AppendLine('<body>');
    
    // Cabeçalho do relatório
    HTMLBuilder.AppendLine('    <header class="report-header">');
    if FConfiguration.Logo <> '' then
      HTMLBuilder.AppendLine('        <img src="' + FConfiguration.Logo + '" alt="Logo" class="logo">');
    HTMLBuilder.AppendLine('        <h1>' + FConfiguration.Title + '</h1>');
    if FConfiguration.Subtitle <> '' then
      HTMLBuilder.AppendLine('        <h2>' + FConfiguration.Subtitle + '</h2>');
    HTMLBuilder.AppendLine('        <p class="report-info">');
    HTMLBuilder.AppendLine('            Gerado em: ' + DateTimeToStr(Now) + '<br>');
    HTMLBuilder.AppendLine('            Autor: ' + FConfiguration.Author + '<br>');
    if FConfiguration.Company <> '' then
      HTMLBuilder.AppendLine('            Empresa: ' + FConfiguration.Company);
    HTMLBuilder.AppendLine('        </p>');
    HTMLBuilder.AppendLine('    </header>');
    
    // Índice (se habilitado)
    if FConfiguration.IncludeTOC then
    begin
      HTMLBuilder.AppendLine('    <nav class="table-of-contents">');
      HTMLBuilder.AppendLine('        <h3>Índice</h3>');
      HTMLBuilder.AppendLine(CreateTableOfContents(Template.Sections));
      HTMLBuilder.AppendLine('    </nav>');
    end;
    
    // Resumo executivo (se dados disponíveis)
    if Data.GetValue('summary') <> nil then
    begin
      HTMLBuilder.AppendLine('    <section class="executive-summary">');
      HTMLBuilder.AppendLine('        <h2>Resumo Executivo</h2>');
      HTMLBuilder.AppendLine('        <div class="summary-content">');
      HTMLBuilder.AppendLine(Data.GetValue<string>('summary'));
      HTMLBuilder.AppendLine('        </div>');
      HTMLBuilder.AppendLine('    </section>');
    end;
    
    // Conteúdo principal - seções
    HTMLBuilder.AppendLine('    <main class="report-content">');
    
    for I := 0 to High(Template.Sections) do
    begin
      Section := Template.Sections[I];
      if not Section.Visible then
        Continue;
        
      HTMLBuilder.AppendLine('        <section class="report-section" id="section-' + IntToStr(I) + '">');
      HTMLBuilder.AppendLine('            <h2>' + Section.Title + '</h2>');
      
      case Section.SectionType of
        'text':
        begin
          SectionContent := ProcessTextSection(Section, Data);
          HTMLBuilder.AppendLine('            <div class="text-content">');
          HTMLBuilder.AppendLine(SectionContent);
          HTMLBuilder.AppendLine('            </div>');
        end;
        
        'chart':
        begin
          if FConfiguration.IncludeCharts then
          begin
            SectionContent := CreateChart(Section.ChartType, Section.ChartData, Section.Title);
            HTMLBuilder.AppendLine('            <div class="chart-content">');
            HTMLBuilder.AppendLine(SectionContent);
            HTMLBuilder.AppendLine('            </div>');
          end;
        end;
        
        'table':
        begin
          SectionContent := CreateHTMLTable(Section.Data);
          HTMLBuilder.AppendLine('            <div class="table-content">');
          HTMLBuilder.AppendLine(SectionContent);
          HTMLBuilder.AppendLine('            </div>');
        end;
        
        'metrics':
        begin
          if FConfiguration.IncludeStatistics then
          begin
            SectionContent := CreateMetricsSection(Data);
            HTMLBuilder.AppendLine('            <div class="metrics-content">');
            HTMLBuilder.AppendLine(SectionContent);
            HTMLBuilder.AppendLine('            </div>');
          end;
        end;
      end;
      
      HTMLBuilder.AppendLine('        </section>');
    end;
    
    HTMLBuilder.AppendLine('    </main>');
    
    // Recomendações (se habilitado)
    if FConfiguration.IncludeRecommendations then
    begin
      var Metrics := CalculateMetrics(Data);
      var Recommendations := GenerateRecommendations(Metrics);
      if Recommendations.Count > 0 then
      begin
        HTMLBuilder.AppendLine('    <section class="recommendations">');
        HTMLBuilder.AppendLine('        <h2>Recomendações</h2>');
        HTMLBuilder.AppendLine('        <ul>');
        for var Recommendation in Recommendations do
          HTMLBuilder.AppendLine('            <li>' + Recommendation + '</li>');
        HTMLBuilder.AppendLine('        </ul>');
        HTMLBuilder.AppendLine('    </section>');
      end;
    end;
    
    // Rodapé
    HTMLBuilder.AppendLine('    <footer class="report-footer">');
    HTMLBuilder.AppendLine('        <p>Relatório gerado automaticamente pelo OllamaTrainer</p>');
    if FWatermarkEnabled then
      HTMLBuilder.AppendLine('        <p class="watermark">' + FWatermarkText + '</p>');
    HTMLBuilder.AppendLine('        <p class="generation-info">');
    HTMLBuilder.AppendLine('            Data de geração: ' + DateTimeToStr(Now) + '<br>');
    HTMLBuilder.AppendLine('            Versão: 2.0');
    HTMLBuilder.AppendLine('        </p>');
    HTMLBuilder.AppendLine('    </footer>');
    
    // Scripts JavaScript (se necessário para gráficos)
    HTMLBuilder.AppendLine('    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>');
    HTMLBuilder.AppendLine('    <script>');
    HTMLBuilder.AppendLine('        // Inicializar gráficos dinâmicos');
    HTMLBuilder.AppendLine('        document.addEventListener("DOMContentLoaded", function() {');
    HTMLBuilder.AppendLine('            initializeCharts();');
    HTMLBuilder.AppendLine('        });');
    HTMLBuilder.AppendLine('    </script>');
    
    HTMLBuilder.AppendLine('</body>');
    HTMLBuilder.AppendLine('</html>');
    
    Result := HTMLBuilder.ToString;
    
  finally
    HTMLBuilder.Free;
  end;
end;

function TReportGenerator.GetCSSStyles(const Theme: string): string;
begin
  Result := '''
    /* Estilos base do relatório */
    * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
    }
    
    body {
        font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
        line-height: 1.6;
        color: #333;
        background-color: #f8f9fa;
    }
    
    .report-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 2rem;
        text-align: center;
        margin-bottom: 2rem;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    
    .report-header h1 {
        font-size: 2.5rem;
        margin-bottom: 0.5rem;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
    }
    
    .report-header h2 {
        font-size: 1.5rem;
        font-weight: 300;
        margin-bottom: 1rem;
    }
    
    .logo {
        max-height: 80px;
        margin-bottom: 1rem;
    }
    
    .report-info {
        font-size: 1rem;
        opacity: 0.9;
    }
    
    .table-of-contents {
        background: white;
        padding: 1.5rem;
        margin: 0 2rem 2rem 2rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    .table-of-contents h3 {
        color: #667eea;
        margin-bottom: 1rem;
        border-bottom: 2px solid #667eea;
        padding-bottom: 0.5rem;
    }
    
    .report-content {
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 2rem;
    }
    
    .report-section {
        background: white;
        margin-bottom: 2rem;
        padding: 2rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border-left: 4px solid #667eea;
    }
    
    .report-section h2 {
        color: #667eea;
        margin-bottom: 1.5rem;
        font-size: 1.8rem;
        border-bottom: 1px solid #e9ecef;
        padding-bottom: 0.5rem;
    }
    
    .text-content {
        line-height: 1.8;
        color: #555;
    }
    
    .chart-content {
        margin: 1rem 0;
        text-align: center;
    }
    
    .table-content {
        overflow-x: auto;
    }
    
    .metrics-content {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 1rem;
        margin: 1rem 0;
    }
    
    .metric-card {
        background: #f8f9fa;
        padding: 1.5rem;
        border-radius: 8px;
        border-left: 4px solid #28a745;
        text-align: center;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    .metric-card.warning {
        border-left-color: #ffc107;
    }
    
    .metric-card.critical {
        border-left-color: #dc3545;
    }
    
    .metric-value {
        font-size: 2rem;
        font-weight: bold;
        color: #667eea;
        margin-bottom: 0.5rem;
    }
    
    .metric-name {
        font-size: 0.9rem;
        color: #6c757d;
        text-transform: uppercase;
        letter-spacing: 1px;
    }
    
    .metric-trend {
        font-size: 0.8rem;
        margin-top: 0.5rem;
    }
    
    .metric-trend.up {
        color: #28a745;
    }
    
    .metric-trend.down {
        color: #dc3545;
    }
    
    .recommendations {
        background: #e3f2fd;
        padding: 2rem;
        margin: 2rem;
        border-radius: 8px;
        border-left: 4px solid #2196f3;
    }
    
    .recommendations h2 {
        color: #1565c0;
        margin-bottom: 1rem;
    }
    
    .recommendations ul {
        list-style-type: none;
        padding-left: 0;
    }
    
    .recommendations li {
        background: white;
        padding: 1rem;
        margin-bottom: 0.5rem;
        border-radius: 4px;
        border-left: 3px solid #2196f3;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1);
    }
    
    .report-footer {
        background: #343a40;
        color: white;
        text-align: center;
        padding: 2rem;
        margin-top: 3rem;
    }
    
    .watermark {
        opacity: 0.7;
        font-style: italic;
    }
    
    .generation-info {
        font-size: 0.8rem;
        opacity: 0.8;
        margin-top: 1rem;
    }
    
    /* Estilos para tabelas */
    table {
        width: 100%;
        border-collapse: collapse;
        margin: 1rem 0;
    }
    
    th, td {
        padding: 0.75rem;
        text-align: left;
        border-bottom: 1px solid #dee2e6;
    }
    
    th {
        background-color: #f8f9fa;
        font-weight: 600;
        color: #495057;
        border-top: 1px solid #dee2e6;
    }
    
    tr:hover {
        background-color: #f8f9fa;
    }
    
    /* Responsividade */
    @media (max-width: 768px) {
        .report-content {
            padding: 0 1rem;
        }
        
        .report-section {
            padding: 1rem;
        }
        
        .report-header h1 {
            font-size: 2rem;
        }
        
        .metrics-content {
            grid-template-columns: 1fr;
        }
    }
    
    /* Estilos para impressão */
    @media print {
        body {
            background: white;
        }
        
        .report-section {
            box-shadow: none;
            border: 1px solid #ddd;
            page-break-inside: avoid;
        }
        
        .report-header {
            background: #667eea !important;
            -webkit-print-color-adjust: exact;
        }
    }
  ''';
  
  // Aplicar tema específico
  case LowerCase(Theme) of
    'dark':
      Result := Result + '''
        body { background-color: #1a1a1a; color: #e0e0e0; }
        .report-section { background: #2d2d2d; color: #e0e0e0; }
        .table-of-contents { background: #2d2d2d; color: #e0e0e0; }
      ''';
    'corporate':
      Result := Result + '''
        .report-header { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); }
        .report-section { border-left-color: #2c3e50; }
        .report-section h2 { color: #2c3e50; }
      ''';
  end;
end;

// Implementações de métodos auxiliares

function TReportGenerator.LoadTemplate(const TemplateName: string): TReportTemplate;
begin
  if FTemplates.TryGetValue(TemplateName, Result) then
    Exit;
  
  // Template padrão se não encontrado
  Result := CreateDefaultTemplate(TemplateName);
  FTemplates.Add(TemplateName, Result);
end;

function TReportGenerator.CreateDefaultTemplate(const TemplateName: string): TReportTemplate;
begin
  Result.Name := TemplateName;
  Result.Description := 'Template padrão para ' + TemplateName;
  Result.Author := 'OllamaTrainer';
  Result.Version := '1.0';
  Result.CreatedDate := Now;
  Result.Parameters := TJSONObject.Create;
  
  // Criar seções básicas baseado no tipo
  if TemplateName.Contains('training') then
  begin
    SetLength(Result.Sections, 4);
    
    Result.Sections[0].Title := 'Visão Geral do Treinamento';
    Result.Sections[0].SectionType := 'text';
    Result.Sections[0].Visible := True;
    Result.Sections[0].Order := 1;
    
    Result.Sections[1].Title := 'Progresso por Época';
    Result.Sections[1].SectionType := 'chart';
    Result.Sections[1].ChartType := ctLine;
    Result.Sections[1].Visible := True;
    Result.Sections[1].Order := 2;
    
    Result.Sections[2].Title := 'Métricas de Desempenho';
    Result.Sections[2].SectionType := 'metrics';
    Result.Sections[2].Visible := True;
    Result.Sections[2].Order := 3;
    
    Result.Sections[3].Title := 'Dados Detalhados';
    Result.Sections[3].SectionType := 'table';
    Result.Sections[3].Visible := True;
    Result.Sections[3].Order := 4;
  end;
end;

procedure TReportGenerator.InitializeDefaultTemplates;
begin
  // Inicializar templates padrão para cada tipo de relatório
  LoadTemplate('training_report');
  LoadTemplate('model_comparison');
  LoadTemplate('performance_analysis');
  LoadTemplate('system_health');
  LoadTemplate('usage_statistics');
  LoadTemplate('error_analysis');
end;

procedure TReportGenerator.InitializeDefaultConfiguration;
begin
  FConfiguration.Title := 'Relatório OllamaTrainer';
  FConfiguration.Subtitle := 'Análise de Treinamento de IA';
  FConfiguration.Author := 'Sistema OllamaTrainer';
  FConfiguration.Company := '';
  FConfiguration.Logo := '';
  FConfiguration.Theme := 'default';
  FConfiguration.IncludeTOC := True;
  FConfiguration.IncludeCharts := True;
  FConfiguration.IncludeStatistics := True;
  FConfiguration.IncludeRecommendations := True;
  FConfiguration.DateRange.StartDate := Now - 30;
  FConfiguration.DateRange.EndDate := Now;
  FConfiguration.Filters := TJSONObject.Create;
  FConfiguration.CustomFields := TJSONObject.Create;
end;

function TReportGenerator.CollectTrainingData: TJSONObject;
begin
  Result := TJSONObject.Create;
  
  try
    // Coletar dados do TrainingEngine se disponível
    if Assigned(FTrainingEngine) then
    begin
      Result.AddPair('state', GetEnumName(TypeInfo(TTrainingState), Ord(FTrainingEngine.State)));
      Result.AddPair('current_epoch', TJSONNumber.Create(FTrainingEngine.CurrentEpoch));
      Result.AddPair('total_epochs', TJSONNumber.Create(FTrainingEngine.TotalEpochs));
      Result.AddPair('overall_progress', TJSONNumber.Create(FTrainingEngine.OverallProgress));
      Result.AddPair('training_id', FTrainingEngine.TrainingID);
      
      // Adicionar métricas históricas se disponíveis
      if FTrainingEngine.MetricsHistory.Count > 0 then
      begin
        var MetricsArray := TJSONArray.Create;
        for var Metric in FTrainingEngine.MetricsHistory do
        begin
          var MetricObj := TJSONObject.Create;
          MetricObj.AddPair('epoch', TJSONNumber.Create(Metric.Epoch));
          MetricObj.AddPair('training_loss', TJSONNumber.Create(Metric.TrainingLoss));
          MetricObj.AddPair('validation_loss', TJSONNumber.Create(Metric.ValidationLoss));
          MetricObj.AddPair('training_accuracy', TJSONNumber.Create(Metric.TrainingAccuracy));
          MetricObj.AddPair('validation_accuracy', TJSONNumber.Create(Metric.ValidationAccuracy));
          MetricObj.AddPair('learning_rate', TJSONNumber.Create(Metric.LearningRate));
          MetricObj.AddPair('time_elapsed', DateToISO8601(Metric.TimeElapsed));
          MetricObj.AddPair('memory_usage', TJSONNumber.Create(Metric.MemoryUsage));
          
          MetricsArray.AddElement(MetricObj);
        end;
        Result.AddPair('metrics_history', MetricsArray);
      end;
    end;
    
    // Adicionar timestamp e metadata
    Result.AddPair('collected_at', DateToISO8601(Now));
    Result.AddPair('data_source', 'TrainingEngine');
    
  except
    on E: Exception do
    begin
      Result.AddPair('error', 'Erro ao coletar dados: ' + E.Message);
      if Assigned(FLogger) then
        FLogger.Error('Erro ao coletar dados de treinamento: ' + E.Message, lcData);
    end;
  end;
end;

// Implementações básicas para métodos restantes (placeholders)

function TReportGenerator.GenerateMarkdownReport(const Template: TReportTemplate; const Data: TJSONObject): string;
begin
  Result := '# ' + FConfiguration.Title + #13#10#13#10;
  Result := Result + 'Relatório gerado em: ' + DateTimeToStr(Now) + #13#10#13#10;
  Result := Result + '## Resumo' + #13#10;
  Result := Result + 'Este é um relatório em formato Markdown.' + #13#10;
end;

function TReportGenerator.GenerateJSONReport(const Template: TReportTemplate; const Data: TJSONObject): string;
var
  ReportObj: TJSONObject;
begin
  ReportObj := TJSONObject.Create;
  try
    ReportObj.AddPair('title', FConfiguration.Title);
    ReportObj.AddPair('generated_at', DateToISO8601(Now));
    ReportObj.AddPair('author', FConfiguration.Author);
    ReportObj.AddPair('data', Data.Clone as TJSONObject);
    
    Result := ReportObj.ToString;
  finally
    ReportObj.Free;
  end;
end;

function TReportGenerator.GenerateCSVReport(const Template: TReportTemplate; const Data: TJSONObject): string;
begin
  Result := 'Timestamp,Metric,Value,Unit' + #13#10;
  Result := Result + DateTimeToStr(Now) + ',Report Generated,1,Count' + #13#10;
end;

procedure TReportGenerator.UpdateProgress(Progress: Integer; const Status: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Status);
end;

procedure TReportGenerator.LogReportGeneration(const ReportType: string; Success: Boolean; const Details: string);
begin
  if Assigned(FLogger) then
  begin
    if Success then
      FLogger.Info(Format('Relatório gerado: %s - %s', [ReportType, Details]), lcGeneral)
    else
      FLogger.Error(Format('Falha na geração: %s - %s', [ReportType, Details]), lcGeneral);
  end;
end;

// Funções auxiliares

function GetFormatExtension(Format: TReportFormat): string;
begin
  case Format of
    rfHTML: Result := 'html';
    rfPDF: Result := 'pdf';
    rfMarkdown: Result := 'md';
    rfJSON: Result := 'json';
    rfCSV: Result := 'csv';
    rfExcel: Result := 'xlsx';
    rfPowerPoint: Result := 'pptx';
  else
    Result := 'html';
  end;
end;

// Implementações simplificadas para métodos restantes
function TReportGenerator.CalculateComparisonMetrics(const ModelsData: TJSONArray): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('total_models', TJSONNumber.Create(ModelsData.Count));
  Result.AddPair('comparison_completed', TJSONBool.Create(True));
end;

function TReportGenerator.ProcessTextSection(const Section: TReportSection; const Data: TJSONObject): string;
begin
  Result := Section.Content;
  if Result = '' then
    Result := 'Conteúdo da seção: ' + Section.Title;
end;

function TReportGenerator.CreateHTMLTable(const Data: TJSONObject): string;
begin
  Result := '<table class="data-table"><tr><th>Propriedade</th><th>Valor</th></tr>';
  for var Pair in Data do
    Result := Result + '<tr><td>' + Pair.JsonString.Value + '</td><td>' + Pair.JsonValue.Value + '</td></tr>';
  Result := Result + '</table>';
end;

function TReportGenerator.CreateMetricsSection(const Data: TJSONObject): string;
begin
  Result := '<div class="metrics-grid">';
  Result := Result + '<div class="metric-card"><div class="metric-value">100%</div><div class="metric-name">Completude</div></div>';
  Result := Result + '<div class="metric-card"><div class="metric-value">95%</div><div class="metric-name">Precisão</div></div>';
  Result := Result + '</div>';
end;

// Métodos públicos básicos
function TReportGenerator.GeneratePerformanceReport(StartDate, EndDate: TDateTime; Format: TReportFormat): string;
begin
  Result := GenerateCustomReport('performance_analysis', 
    TJSONObject.Create.AddPair('start_date', DateToISO8601(StartDate))
                     .AddPair('end_date', DateToISO8601(EndDate)), Format);
end;

function TReportGenerator.GenerateSystemHealthReport(Format: TReportFormat): string;
begin
  Result := GenerateCustomReport('system_health', TJSONObject.Create, Format);
end;

function TReportGenerator.GenerateCustomReport(const TemplateName: string; const Parameters: TJSONObject; Format: TReportFormat): string;
begin
  Result := '';
  // Implementação básica
  try
    var Template := LoadTemplate(TemplateName);
    var Data := TJSONObject.Create;
    try
      Data.AddPair('parameters', Parameters.Clone as TJSONObject);
      Data.AddPair('template', TemplateName);
      
      case Format of
        rfHTML: Result := GenerateHTMLReport(Template, Data);
        rfJSON: Result := GenerateJSONReport(Template, Data);
      else
        Result := GenerateHTMLReport(Template, Data);
      end;
      
      // Salvar arquivo
      var FileName := TPath.Combine(FOutputPath, 
        Format('custom_report_%s_%s.%s', [TemplateName, FormatDateTime('yyyymmdd_hhnnss', Now), 
          GetFormatExtension(Format)]));
      
      TFile.WriteAllText(FileName, Result, TEncoding.UTF8);
      FGeneratedReports.Add(FileName);
      Result := FileName;
      
    finally
      Data.Free;
    end;
  except
    on E: Exception do
    begin
      LogReportGeneration('Custom Report', False, E.Message);
    end;
  end;
end;

end.