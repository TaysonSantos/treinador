unit DataProcessor;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Generics.Collections,
  System.JSON, System.RegularExpressions, System.StrUtils, System.Math,
  Vcl.Graphics, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage;

type
  TTrainingDataType = (tdtText, tdtCode, tdtImage, tdtJSON, tdtCSV, tdtMarkdown);
  
  TTrainingItem = class
  public
    Input: string;
    ExpectedOutput: string;
    PredictedOutput: string;
    Loss: Double;
    Confidence: Double;
    Metadata: TJSONObject;
    
    constructor Create;
    destructor Destroy; override;
  end;
  
  TTrainingBatch = class
  public
    Items: TObjectList<TTrainingItem>;
    BatchIndex: Integer;
    BatchSize: Integer;
    DataType: TTrainingDataType;
    
    constructor Create;
    destructor Destroy; override;
  end;
  
  TTrainingBatchList = TObjectList<TTrainingBatch>;
  
  TCodeLanguage = (clPascal, clPython, clJavaScript, clJava, clCSharp, clCPP, 
    clC, clGo, clRust, clTypeScript, clPHP, clRuby, clSwift, clKotlin, clSQL, clOther);
    
  TDataAugmentationOptions = record
    UseParaphrasing: Boolean;
    UseSynonymReplacement: Boolean;
    UseBackTranslation: Boolean;
    UseCodeRefactoring: Boolean;
    UseImageRotation: Boolean;
    UseImageFlip: Boolean;
    UseImageBrightness: Boolean;
    AugmentationFactor: Double; // Quantos dados aumentados gerar (1.0 = 100% mais dados)
  end;
  
  TDataStatistics = record
    TotalFiles: Integer;
    TotalItems: Integer;
    LanguageDistribution: TDictionary<string, Integer>;
    FileSizeDistribution: TDictionary<string, Integer>;
    DataQualityScore: Double;
    AverageTokenLength: Double;
    UniqueTokens: Integer;
  end;
  
  TDataProcessor = class
  private
    FCurrentDirectory: string;
    FFileFilters: TStringList;
    FCodeLanguages: TDictionary<string, TCodeLanguage>;
    FTokenizer: TRegEx;
    FAugmentationOptions: TDataAugmentationOptions;
    FMaxFileSize: Int64;
    FMinFileSize: Int64;
    FProcessedFiles: TStringList;
    FErrorFiles: TStringList;
    
    // Cache para performance
    FTrainingBatches: TTrainingBatchList;
    FValidationBatches: TTrainingBatchList;
    FProcessedData: TObjectList<TTrainingItem>;
    
    function DetectFileType(const FileName: string): TTrainingDataType;
    function DetectCodeLanguage(const FileName, Content: string): TCodeLanguage;
    function IsValidTrainingFile(const FileName: string): Boolean;
    function LoadFileContent(const FileName: string): string;
    function CleanText(const Text: string): string;
    function TokenizeText(const Text: string): TArray<string>;
    function ExtractCodeSnippets(const Content: string; Language: TCodeLanguage): TStringList;
    function ExtractFunctions(const Content: string; Language: TCodeLanguage): TStringList;
    function ExtractComments(const Content: string; Language: TCodeLanguage): TStringList;
    function CreatePromptResponse(const Input, Output: string; DataType: TTrainingDataType): TTrainingItem;
    function AugmentTextData(const OriginalText: string): TStringList;
    function AugmentCodeData(const OriginalCode: string; Language: TCodeLanguage): TStringList;
    function ValidateTrainingItem(const Item: TTrainingItem): Boolean;
    function CalculateQualityScore(const Content: string): Double;
    procedure ProcessTextFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure ProcessCodeFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure ProcessImageFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure ProcessJSONFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure ProcessCSVFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure ProcessMarkdownFile(const FileName: string; Items: TObjectList<TTrainingItem>);
    procedure SplitIntoSentences(const Text: string; Sentences: TStringList);
    procedure SplitIntoParagraphs(const Text: string; Paragraphs: TStringList);
    procedure GenerateQAFromText(const Text: string; Items: TObjectList<TTrainingItem>);
    procedure GenerateCodeExamples(const Code: string; Language: TCodeLanguage; Items: TObjectList<TTrainingItem>);
    procedure ShuffleBatches(Batches: TTrainingBatchList);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Propriedades
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property AugmentationOptions: TDataAugmentationOptions read FAugmentationOptions write FAugmentationOptions;
    property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
    property MinFileSize: Int64 read FMinFileSize write FMinFileSize;
    property ProcessedFiles: TStringList read FProcessedFiles;
    property ErrorFiles: TStringList read FErrorFiles;
    
    // Métodos principais
    function CountTrainingFiles(const Directory: string; Recursive: Boolean = True): Integer;
    procedure ScanDataDirectory(const Directory: string; Files: TStringList; Recursive: Boolean = True);
    function PrepareCodeTrainingData(const Files: TStringList): Boolean;
    function PrepareTextTrainingData(const Files: TStringList): Boolean;
    function PrepareImageTrainingData(const Files: TStringList): Boolean;
    function PrepareGeneralTrainingData(const Files: TStringList): Boolean;
    
    // Métodos de batch
    function GetTrainingBatches(BatchSize: Integer; TrainingType: TTrainingType): TTrainingBatchList;
    function GetValidationBatches(BatchSize: Integer; TrainingType: TTrainingType): TTrainingBatchList;
    function CreateBatchesFromItems(Items: TObjectList<TTrainingItem>; BatchSize: Integer): TTrainingBatchList;
    
    // Análise e estatísticas
    function AnalyzeDataset(const Directory: string): TDataStatistics;
    function GetDataQualityReport: string;
    function ValidateDataIntegrity: Boolean;
    
    // Augmentação de dados
    function AugmentTrainingData(const Items: TObjectList<TTrainingItem>): TObjectList<TTrainingItem>;
    procedure ApplyDataAugmentation(var Batches: TTrainingBatchList);
    
    // Métodos utilitários
    function LoadFromCache(const CacheFile: string): Boolean;
    function SaveToCache(const CacheFile: string): Boolean;
    procedure ClearCache;
    function GetSupportedFormats: TArray<string>;
    function EstimateProcessingTime(const Files: TStringList): TDateTime;
    
    // Configuração
    procedure AddFileFilter(const Extension: string);
    procedure RemoveFileFilter(const Extension: string);
    procedure ConfigureForLanguage(Language: TCodeLanguage);
    procedure SetDefaultAugmentationOptions;
  end;

implementation

uses
  System.Variants, System.NetEncoding, System.Hash, System.DateUtils;

{ TTrainingItem }

constructor TTrainingItem.Create;
begin
  inherited Create;
  Metadata := TJSONObject.Create;
  Loss := 0.0;
  Confidence := 0.0;
end;

destructor TTrainingItem.Destroy;
begin
  Metadata.Free;
  inherited Destroy;
end;

{ TTrainingBatch }

constructor TTrainingBatch.Create;
begin
  inherited Create;
  Items := TObjectList<TTrainingItem>.Create;
  BatchIndex := 0;
  BatchSize := 0;
  DataType := tdtText;
end;

destructor TTrainingBatch.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

{ TDataProcessor }

constructor TDataProcessor.Create;
begin
  inherited Create;
  
  FFileFilters := TStringList.Create;
  FCodeLanguages := TDictionary<string, TCodeLanguage>.Create;
  FProcessedFiles := TStringList.Create;
  FErrorFiles := TStringList.Create;
  
  FTrainingBatches := TTrainingBatchList.Create;
  FValidationBatches := TTrainingBatchList.Create;
  FProcessedData := TObjectList<TTrainingItem>.Create;
  
  // Configurar filtros padrão
  FFileFilters.Add('.txt');
  FFileFilters.Add('.pas');
  FFileFilters.Add('.py');
  FFileFilters.Add('.js');
  FFileFilters.Add('.java');
  FFileFilters.Add('.cs');
  FFileFilters.Add('.cpp');
  FFileFilters.Add('.c');
  FFileFilters.Add('.go');
  FFileFilters.Add('.rs');
  FFileFilters.Add('.ts');
  FFileFilters.Add('.php');
  FFileFilters.Add('.rb');
  FFileFilters.Add('.swift');
  FFileFilters.Add('.kt');
  FFileFilters.Add('.sql');
  FFileFilters.Add('.json');
  FFileFilters.Add('.csv');
  FFileFilters.Add('.md');
  FFileFilters.Add('.jpg');
  FFileFilters.Add('.jpeg');
  FFileFilters.Add('.png');
  FFileFilters.Add('.bmp');
  
  // Mapear extensões para linguagens
  FCodeLanguages.Add('.pas', clPascal);
  FCodeLanguages.Add('.pp', clPascal);
  FCodeLanguages.Add('.py', clPython);
  FCodeLanguages.Add('.js', clJavaScript);
  FCodeLanguages.Add('.java', clJava);
  FCodeLanguages.Add('.cs', clCSharp);
  FCodeLanguages.Add('.cpp', clCPP);
  FCodeLanguages.Add('.cxx', clCPP);
  FCodeLanguages.Add('.cc', clCPP);
  FCodeLanguages.Add('.c', clC);
  FCodeLanguages.Add('.go', clGo);
  FCodeLanguages.Add('.rs', clRust);
  FCodeLanguages.Add('.ts', clTypeScript);
  FCodeLanguages.Add('.php', clPHP);
  FCodeLanguages.Add('.rb', clRuby);
  FCodeLanguages.Add('.swift', clSwift);
  FCodeLanguages.Add('.kt', clKotlin);
  FCodeLanguages.Add('.sql', clSQL);
  
  // Configurar tokenizer
  FTokenizer := TRegEx.Create('\b\w+\b');
  
  // Configurações padrão
  FMaxFileSize := 10 * 1024 * 1024; // 10MB
  FMinFileSize := 10; // 10 bytes
  
  SetDefaultAugmentationOptions;
end;

destructor TDataProcessor.Destroy;
begin
  ClearCache;
  FProcessedData.Free;
  FValidationBatches.Free;
  FTrainingBatches.Free;
  FErrorFiles.Free;
  FProcessedFiles.Free;
  FCodeLanguages.Free;
  FFileFilters.Free;
  inherited Destroy;
end;

function TDataProcessor.CountTrainingFiles(const Directory: string; Recursive: Boolean): Integer;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    ScanDataDirectory(Directory, Files, Recursive);
    Result := Files.Count;
  finally
    Files.Free;
  end;
end;

procedure TDataProcessor.ScanDataDirectory(const Directory: string; Files: TStringList; Recursive: Boolean);
var
  SearchOption: TSearchOption;
  FoundFiles: TStringDynArray;
  I: Integer;
  Extension: string;
begin
  Files.Clear;
  FCurrentDirectory := Directory;
  
  if not DirectoryExists(Directory) then
    raise Exception.CreateFmt('Diretório não existe: %s', [Directory]);
  
  if Recursive then
    SearchOption := TSearchOption.soAllDirectories
  else
    SearchOption := TSearchOption.soTopDirectoryOnly;
    
  FoundFiles := TDirectory.GetFiles(Directory, '*.*', SearchOption);
  
  for I := 0 to High(FoundFiles) do
  begin
    Extension := LowerCase(ExtractFileExt(FoundFiles[I]));
    if FFileFilters.IndexOf(Extension) >= 0 then
    begin
      if IsValidTrainingFile(FoundFiles[I]) then
        Files.Add(FoundFiles[I]);
    end;
  end;
end;

function TDataProcessor.IsValidTrainingFile(const FileName: string): Boolean;
var
  FileSize: Int64;
begin
  Result := False;
  
  if not FileExists(FileName) then
    Exit;
    
  FileSize := TFile.GetSize(FileName);
  Result := (FileSize >= FMinFileSize) and (FileSize <= FMaxFileSize);
end;

function TDataProcessor.DetectFileType(const FileName: string): TTrainingDataType;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(FileName));
  
  if Extension = '.json' then
    Result := tdtJSON
  else if Extension = '.csv' then
    Result := tdtCSV
  else if Extension = '.md' then
    Result := tdtMarkdown
  else if (Extension = '.jpg') or (Extension = '.jpeg') or 
          (Extension = '.png') or (Extension = '.bmp') then
    Result := tdtImage
  else if FCodeLanguages.ContainsKey(Extension) then
    Result := tdtCode
  else
    Result := tdtText;
end;

function TDataProcessor.DetectCodeLanguage(const FileName, Content: string): TCodeLanguage;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(FileName));
  
  if FCodeLanguages.TryGetValue(Extension, Result) then
    Exit;
    
  // Detectar por conteúdo se extensão não for reconhecida
  if Content.Contains('function ') or Content.Contains('var ') or Content.Contains('const ') then
    Result := clJavaScript
  else if Content.Contains('def ') or Content.Contains('import ') then
    Result := clPython
  else if Content.Contains('public class') or Content.Contains('private ') then
    Result := clJava
  else if Content.Contains('procedure ') or Content.Contains('function ') or Content.Contains('begin') then
    Result := clPascal
  else
    Result := clOther;
end;

function TDataProcessor.LoadFileContent(const FileName: string): string;
var
  Encoding: TEncoding;
begin
  Result := '';
  
  try
    // Detectar encoding
    Encoding := nil;
    Result := TFile.ReadAllText(FileName, Encoding);
    
    if Result.Length = 0 then
    begin
      FErrorFiles.Add(FileName + ' - Arquivo vazio');
      Exit;
    end;
    
    FProcessedFiles.Add(FileName);
    
  except
    on E: Exception do
    begin
      FErrorFiles.Add(FileName + ' - ' + E.Message);
    end;
  end;
end;

function TDataProcessor.CleanText(const Text: string): string;
var
  CleanedText: string;
begin
  // Remover caracteres especiais e normalizar
  CleanedText := Text;
  
  // Remover caracteres de controle
  CleanedText := TRegEx.Replace(CleanedText, '[\x00-\x1F\x7F]', ' ');
  
  // Normalizar quebras de linha
  CleanedText := StringReplace(CleanedText, #13#10, #10, [rfReplaceAll]);
  CleanedText := StringReplace(CleanedText, #13, #10, [rfReplaceAll]);
  
  // Remover espaços em excesso
  CleanedText := TRegEx.Replace(CleanedText, '\s+', ' ');
  
  // Trim
  CleanedText := Trim(CleanedText);
  
  Result := CleanedText;
end;

function TDataProcessor.TokenizeText(const Text: string): TArray<string>;
var
  Matches: TMatchCollection;
  I: Integer;
begin
  Matches := FTokenizer.Matches(Text);
  SetLength(Result, Matches.Count);
  
  for I := 0 to Matches.Count - 1 do
    Result[I] := Matches[I].Value;
end;

function TDataProcessor.PrepareCodeTrainingData(const Files: TStringList): Boolean;
var
  I: Integer;
  Items: TObjectList<TTrainingItem>;
begin
  Result := False;
  Items := TObjectList<TTrainingItem>.Create;
  
  try
    for I := 0 to Files.Count - 1 do
    begin
      if DetectFileType(Files[I]) = tdtCode then
        ProcessCodeFile(Files[I], Items);
    end;
    
    if Items.Count > 0 then
    begin
      // Augmentar dados se necessário
      if FAugmentationOptions.UseCodeRefactoring then
      begin
        var AugmentedItems := AugmentTrainingData(Items);
        try
          for var Item in AugmentedItems do
            Items.Add(Item);
        finally
          AugmentedItems.Free;
        end;
      end;
      
      // Limpar cache anterior
      FProcessedData.Clear;
      
      // Transferir itens
      for var Item in Items do
        FProcessedData.Add(Item);
      Items.Clear; // Para não liberar os objetos
      
      Result := True;
    end;
    
  finally
    Items.Free;
  end;
end;

function TDataProcessor.PrepareTextTrainingData(const Files: TStringList): Boolean;
var
  I: Integer;
  Items: TObjectList<TTrainingItem>;
begin
  Result := False;
  Items := TObjectList<TTrainingItem>.Create;
  
  try
    for I := 0 to Files.Count - 1 do
    begin
      case DetectFileType(Files[I]) of
        tdtText: ProcessTextFile(Files[I], Items);
        tdtMarkdown: ProcessMarkdownFile(Files[I], Items);
        tdtJSON: ProcessJSONFile(Files[I], Items);
        tdtCSV: ProcessCSVFile(Files[I], Items);
      end;
    end;
    
    if Items.Count > 0 then
    begin
      // Augmentar dados se necessário
      if FAugmentationOptions.UseParaphrasing or FAugmentationOptions.UseSynonymReplacement then
      begin
        var AugmentedItems := AugmentTrainingData(Items);
        try
          for var Item in AugmentedItems do
            Items.Add(Item);
        finally
          AugmentedItems.Free;
        end;
      end;
      
      // Limpar cache anterior
      FProcessedData.Clear;
      
      // Transferir itens
      for var Item in Items do
        FProcessedData.Add(Item);
      Items.Clear; // Para não liberar os objetos
      
      Result := True;
    end;
    
  finally
    Items.Free;
  end;
end;

function TDataProcessor.PrepareImageTrainingData(const Files: TStringList): Boolean;
var
  I: Integer;
  Items: TObjectList<TTrainingItem>;
begin
  Result := False;
  Items := TObjectList<TTrainingItem>.Create;
  
  try
    for I := 0 to Files.Count - 1 do
    begin
      if DetectFileType(Files[I]) = tdtImage then
        ProcessImageFile(Files[I], Items);
    end;
    
    if Items.Count > 0 then
    begin
      // Limpar cache anterior
      FProcessedData.Clear;
      
      // Transferir itens
      for var Item in Items do
        FProcessedData.Add(Item);
      Items.Clear; // Para não liberar os objetos
      
      Result := True;
    end;
    
  finally
    Items.Free;
  end;
end;

function TDataProcessor.PrepareGeneralTrainingData(const Files: TStringList): Boolean;
var
  I: Integer;
  Items: TObjectList<TTrainingItem>;
  FileType: TTrainingDataType;
begin
  Result := False;
  Items := TObjectList<TTrainingItem>.Create;
  
  try
    for I := 0 to Files.Count - 1 do
    begin
      FileType := DetectFileType(Files[I]);
      
      case FileType of
        tdtText: ProcessTextFile(Files[I], Items);
        tdtCode: ProcessCodeFile(Files[I], Items);
        tdtImage: ProcessImageFile(Files[I], Items);
        tdtJSON: ProcessJSONFile(Files[I], Items);
        tdtCSV: ProcessCSVFile(Files[I], Items);
        tdtMarkdown: ProcessMarkdownFile(Files[I], Items);
      end;
    end;
    
    if Items.Count > 0 then
    begin
      // Aplicar augmentação geral
      var AugmentedItems := AugmentTrainingData(Items);
      try
        for var Item in AugmentedItems do
          Items.Add(Item);
      finally
        AugmentedItems.Free;
      end;
      
      // Limpar cache anterior
      FProcessedData.Clear;
      
      // Transferir itens
      for var Item in Items do
        FProcessedData.Add(Item);
      Items.Clear; // Para não liberar os objetos
      
      Result := True;
    end;
    
  finally
    Items.Free;
  end;
end;

procedure TDataProcessor.ProcessTextFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Content: string;
  Paragraphs: TStringList;
  I: Integer;
  Item: TTrainingItem;
begin
  Content := LoadFileContent(FileName);
  if Content.Length = 0 then
    Exit;
    
  Content := CleanText(Content);
  
  Paragraphs := TStringList.Create;
  try
    SplitIntoParagraphs(Content, Paragraphs);
    
    for I := 0 to Paragraphs.Count - 1 do
    begin
      if Length(Trim(Paragraphs[I])) < 50 then // Muito curto
        Continue;
        
      // Gerar Q&A do texto
      GenerateQAFromText(Paragraphs[I], Items);
      
      // Criar item de completamento de texto
      Item := TTrainingItem.Create;
      Item.Input := 'Complete o seguinte texto: ' + Copy(Paragraphs[I], 1, Length(Paragraphs[I]) div 2);
      Item.ExpectedOutput := Copy(Paragraphs[I], (Length(Paragraphs[I]) div 2) + 1, MaxInt);
      Item.Metadata.AddPair('source_file', FileName);
      Item.Metadata.AddPair('type', 'text_completion');
      Item.Metadata.AddPair('quality_score', TJSONNumber.Create(CalculateQualityScore(Paragraphs[I])));
      
      if ValidateTrainingItem(Item) then
        Items.Add(Item)
      else
        Item.Free;
    end;
    
  finally
    Paragraphs.Free;
  end;
end;

procedure TDataProcessor.ProcessCodeFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Content: string;
  Language: TCodeLanguage;
  Functions: TStringList;
  I: Integer;
begin
  Content := LoadFileContent(FileName);
  if Content.Length = 0 then
    Exit;
    
  Language := DetectCodeLanguage(FileName, Content);
  
  Functions := TStringList.Create;
  try
    // Extrair funções do código
    Functions := ExtractFunctions(Content, Language);
    
    for I := 0 to Functions.Count - 1 do
    begin
      GenerateCodeExamples(Functions[I], Language, Items);
    end;
    
    // Gerar exemplos de comentários de código
    var Comments := ExtractComments(Content, Language);
    try
      for I := 0 to Comments.Count - 1 do
      begin
        var Item := TTrainingItem.Create;
        Item.Input := 'Explique este código: ' + Comments[I];
        Item.ExpectedOutput := 'Este código implementa...'; // Placeholder
        Item.Metadata.AddPair('source_file', FileName);
        Item.Metadata.AddPair('language', GetEnumName(TypeInfo(TCodeLanguage), Ord(Language)));
        Item.Metadata.AddPair('type', 'code_explanation');
        
        if ValidateTrainingItem(Item) then
          Items.Add(Item)
        else
          Item.Free;
      end;
    finally
      Comments.Free;
    end;
    
  finally
    Functions.Free;
  end;
end;

procedure TDataProcessor.ProcessImageFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Item: TTrainingItem;
  ImageName: string;
  ImageDescription: string;
begin
  ImageName := ExtractFileName(FileName);
  
  // Gerar descrição baseada no nome do arquivo
  ImageDescription := StringReplace(ChangeFileExt(ImageName, ''), '_', ' ', [rfReplaceAll]);
  ImageDescription := StringReplace(ImageDescription, '-', ' ', [rfReplaceAll]);
  
  Item := TTrainingItem.Create;
  Item.Input := 'Descreva esta imagem: ' + FileName;
  Item.ExpectedOutput := 'Esta imagem mostra ' + ImageDescription;
  Item.Metadata.AddPair('source_file', FileName);
  Item.Metadata.AddPair('type', 'image_description');
  Item.Metadata.AddPair('image_format', LowerCase(ExtractFileExt(FileName)));
  
  if ValidateTrainingItem(Item) then
    Items.Add(Item)
  else
    Item.Free;
end;

procedure TDataProcessor.ProcessJSONFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Content: string;
  JSONData: TJSONValue;
  Item: TTrainingItem;
begin
  Content := LoadFileContent(FileName);
  if Content.Length = 0 then
    Exit;
    
  try
    JSONData := TJSONObject.ParseJSONValue(Content);
    if Assigned(JSONData) then
    try
      // Criar item para explicar estrutura JSON
      Item := TTrainingItem.Create;
      Item.Input := 'Explique a estrutura deste JSON: ' + Copy(Content, 1, 200) + '...';
      Item.ExpectedOutput := 'Este JSON contém...'; // Placeholder
      Item.Metadata.AddPair('source_file', FileName);
      Item.Metadata.AddPair('type', 'json_explanation');
      
      if ValidateTrainingItem(Item) then
        Items.Add(Item)
      else
        Item.Free;
        
    finally
      JSONData.Free;
    end;
    
  except
    on E: Exception do
      FErrorFiles.Add(FileName + ' - JSON inválido: ' + E.Message);
  end;
end;

procedure TDataProcessor.ProcessCSVFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Content: string;
  Lines: TStringList;
  Item: TTrainingItem;
begin
  Content := LoadFileContent(FileName);
  if Content.Length = 0 then
    Exit;
    
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    
    if Lines.Count > 1 then
    begin
      Item := TTrainingItem.Create;
      Item.Input := 'Analise este CSV: ' + Lines[0]; // Header
      Item.ExpectedOutput := 'Este CSV contém dados sobre...'; // Placeholder
      Item.Metadata.AddPair('source_file', FileName);
      Item.Metadata.AddPair('type', 'csv_analysis');
      Item.Metadata.AddPair('row_count', TJSONNumber.Create(Lines.Count));
      
      if ValidateTrainingItem(Item) then
        Items.Add(Item)
      else
        Item.Free;
    end;
    
  finally
    Lines.Free;
  end;
end;

procedure TDataProcessor.ProcessMarkdownFile(const FileName: string; Items: TObjectList<TTrainingItem>);
var
  Content: string;
  Sections: TStringList;
  I: Integer;
  Item: TTrainingItem;
begin
  Content := LoadFileContent(FileName);
  if Content.Length = 0 then
    Exit;
    
  Content := CleanText(Content);
  
  Sections := TStringList.Create;
  try
    // Dividir por seções (cabeçalhos)
    var Lines := TStringList.Create;
    try
      Lines.Text := Content;
      var CurrentSection := '';
      
      for I := 0 to Lines.Count - 1 do
      begin
        if Lines[I].StartsWith('#') then
        begin
          if CurrentSection <> '' then
            Sections.Add(CurrentSection);
          CurrentSection := Lines[I] + #13#10;
        end
        else
          CurrentSection := CurrentSection + Lines[I] + #13#10;
      end;
      
      if CurrentSection <> '' then
        Sections.Add(CurrentSection);
        
    finally
      Lines.Free;
    end;
    
    for I := 0 to Sections.Count - 1 do
    begin
      Item := TTrainingItem.Create;
      Item.Input := 'Resuma esta seção: ' + Copy(Sections[I], 1, 200) + '...';
      Item.ExpectedOutput := 'Esta seção trata de...'; // Placeholder
      Item.Metadata.AddPair('source_file', FileName);
      Item.Metadata.AddPair('type', 'markdown_summary');
      
      if ValidateTrainingItem(Item) then
        Items.Add(Item)
      else
        Item.Free;
    end;
    
  finally
    Sections.Free;
  end;
end;

function TDataProcessor.GetTrainingBatches(BatchSize: Integer; TrainingType: TTrainingType): TTrainingBatchList;
var
  TrainingItems: TObjectList<TTrainingItem>;
  ValidationItems: TObjectList<TTrainingItem>;
  I, SplitIndex: Integer;
begin
  Result := TTrainingBatchList.Create;
  
  if FProcessedData.Count = 0 then
    Exit;
    
  // Dividir dados em treinamento e validação
  TrainingItems := TObjectList<TTrainingItem>.Create(False); // Não possui objetos
  ValidationItems := TObjectList<TTrainingItem>.Create(False);
  
  try
    SplitIndex := Round(FProcessedData.Count * 0.8); // 80% para treinamento
    
    for I := 0 to FProcessedData.Count - 1 do
    begin
      if I < SplitIndex then
        TrainingItems.Add(FProcessedData[I])
      else
        ValidationItems.Add(FProcessedData[I]);
    end;
    
    // Criar batches de treinamento
    var TrainingBatches := CreateBatchesFromItems(TrainingItems, BatchSize);
    try
      for var Batch in TrainingBatches do
        Result.Add(Batch);
    finally
      TrainingBatches.Free;
    end;
    
    // Criar batches de validação e armazenar separadamente
    FValidationBatches.Clear;
    var ValidationBatches := CreateBatchesFromItems(ValidationItems, BatchSize);
    try
      for var Batch in ValidationBatches do
        FValidationBatches.Add(Batch);
    finally
      ValidationBatches.Free;
    end;
    
  finally
    ValidationItems.Free;
    TrainingItems.Free;
  end;
  
  // Embaralhar batches
  ShuffleBatches(Result);
end;

function TDataProcessor.GetValidationBatches(BatchSize: Integer; TrainingType: TTrainingType): TTrainingBatchList;
begin
  Result := TTrainingBatchList.Create;
  
  // Retornar cópia dos batches de validação
  for var Batch in FValidationBatches do
  begin
    var NewBatch := TTrainingBatch.Create;
    NewBatch.BatchIndex := Batch.BatchIndex;
    NewBatch.BatchSize := Batch.BatchSize;
    NewBatch.DataType := Batch.DataType;
    
    for var Item in Batch.Items do
    begin
      var NewItem := TTrainingItem.Create;
      NewItem.Input := Item.Input;
      NewItem.ExpectedOutput := Item.ExpectedOutput;
      NewItem.PredictedOutput := Item.PredictedOutput;
      NewItem.Loss := Item.Loss;
      NewItem.Confidence := Item.Confidence;
      NewItem.Metadata := TJSONObject(Item.Metadata.Clone);
      
      NewBatch.Items.Add(NewItem);
    end;
    
    Result.Add(NewBatch);
  end;
end;

function TDataProcessor.CreateBatchesFromItems(Items: TObjectList<TTrainingItem>; BatchSize: Integer): TTrainingBatchList;
var
  I, BatchIndex: Integer;
  CurrentBatch: TTrainingBatch;
begin
  Result := TTrainingBatchList.Create;
  
  if Items.Count = 0 then
    Exit;
    
  CurrentBatch := nil;
  BatchIndex := 0;
  
  for I := 0 to Items.Count - 1 do
  begin
    if (I mod BatchSize) = 0 then
    begin
      CurrentBatch := TTrainingBatch.Create;
      CurrentBatch.BatchIndex := BatchIndex;
      CurrentBatch.BatchSize := BatchSize;
      CurrentBatch.DataType := tdtText; // Default
      
      Result.Add(CurrentBatch);
      Inc(BatchIndex);
    end;
    
    if Assigned(CurrentBatch) then
      CurrentBatch.Items.Add(Items[I]);
  end;
end;

procedure TDataProcessor.ShuffleBatches(Batches: TTrainingBatchList);
var
  I, J: Integer;
begin
  // Algoritmo Fisher-Yates para embaralhar
  for I := Batches.Count - 1 downto 1 do
  begin
    J := Random(I + 1);
    Batches.Exchange(I, J);
  end;
end;

function TDataProcessor.ValidateTrainingItem(const Item: TTrainingItem): Boolean;
begin
  Result := (Trim(Item.Input) <> '') and 
            (Trim(Item.ExpectedOutput) <> '') and
            (Length(Item.Input) >= 10) and
            (Length(Item.ExpectedOutput) >= 5);
end;

function TDataProcessor.CalculateQualityScore(const Content: string): Double;
var
  Score: Double;
  WordCount, SentenceCount: Integer;
  Tokens: TArray<string>;
begin
  Score := 0.0;
  
  // Pontuação baseada em comprimento
  if Length(Content) > 100 then
    Score := Score + 0.3;
    
  // Pontuação baseada em variedade de palavras
  Tokens := TokenizeText(Content);
  WordCount := Length(Tokens);
  
  if WordCount > 20 then
    Score := Score + 0.3;
    
  // Pontuação baseada em estrutura de frases
  SentenceCount := Content.CountChar('.') + Content.CountChar('!') + Content.CountChar('?');
  
  if SentenceCount > 2 then
    Score := Score + 0.2;
    
  // Pontuação baseada em caracteres especiais (indica formatação)
  if Content.Contains(':') or Content.Contains(';') then
    Score := Score + 0.1;
    
  // Penalizar texto muito curto ou muito longo
  if (Length(Content) < 50) or (Length(Content) > 5000) then
    Score := Score - 0.2;
    
  Result := Max(0.0, Min(1.0, Score));
end;

// Implementações de métodos auxiliares e utilitários...

procedure TDataProcessor.SplitIntoSentences(const Text: string; Sentences: TStringList);
var
  Regex: TRegEx;
  Matches: TMatchCollection;
  I: Integer;
  LastEnd: Integer;
  Sentence: string;
begin
  Sentences.Clear;
  
  // Regex para dividir em sentenças
  Regex := TRegEx.Create('[.!?]+\s+');
  Matches := Regex.Matches(Text);
  
  LastEnd := 1;
  for I := 0 to Matches.Count - 1 do
  begin
    Sentence := Copy(Text, LastEnd, Matches[I].Index - LastEnd + Matches[I].Length);
    Sentence := Trim(Sentence);
    if Sentence <> '' then
      Sentences.Add(Sentence);
    LastEnd := Matches[I].Index + Matches[I].Length;
  end;
  
  // Adicionar última sentença se existir
  if LastEnd <= Length(Text) then
  begin
    Sentence := Trim(Copy(Text, LastEnd, MaxInt));
    if Sentence <> '' then
      Sentences.Add(Sentence);
  end;
end;

procedure TDataProcessor.SplitIntoParagraphs(const Text: string; Paragraphs: TStringList);
begin
  Paragraphs.Clear;
  Paragraphs.Text := Text;
  
  // Remover linhas vazias
  for var I := Paragraphs.Count - 1 downto 0 do
  begin
    if Trim(Paragraphs[I]) = '' then
      Paragraphs.Delete(I);
  end;
end;

procedure TDataProcessor.GenerateQAFromText(const Text: string; Items: TObjectList<TTrainingItem>);
var
  Item: TTrainingItem;
begin
  // Gerar pergunta simples baseada no texto
  Item := TTrainingItem.Create;
  Item.Input := 'Sobre o que trata este texto?';
  Item.ExpectedOutput := Copy(Text, 1, 200) + '...';
  Item.Metadata.AddPair('type', 'text_qa');
  
  if ValidateTrainingItem(Item) then
    Items.Add(Item)
  else
    Item.Free;
end;

procedure TDataProcessor.GenerateCodeExamples(const Code: string; Language: TCodeLanguage; Items: TObjectList<TTrainingItem>);
var
  Item: TTrainingItem;
begin
  // Gerar exemplo de código
  Item := TTrainingItem.Create;
  Item.Input := Format('Escreva uma função em %s que faça:', [GetEnumName(TypeInfo(TCodeLanguage), Ord(Language))]);
  Item.ExpectedOutput := Code;
  Item.Metadata.AddPair('type', 'code_generation');
  Item.Metadata.AddPair('language', GetEnumName(TypeInfo(TCodeLanguage), Ord(Language)));
  
  if ValidateTrainingItem(Item) then
    Items.Add(Item)
  else
    Item.Free;
end;

function TDataProcessor.ExtractFunctions(const Content: string; Language: TCodeLanguage): TStringList;
begin
  Result := TStringList.Create;
  
  // Implementação simplificada - detectar funções básicas
  case Language of
    clPascal:
    begin
      var Regex := TRegEx.Create('(function|procedure)\s+\w+.*?end;', [roIgnoreCase, roSingleLine]);
      var Matches := Regex.Matches(Content);
      for var Match in Matches do
        Result.Add(Match.Value);
    end;
    
    clPython:
    begin
      var Regex := TRegEx.Create('def\s+\w+.*?(?=\ndef|\Z)', [roIgnoreCase, roMultiLine]);
      var Matches := Regex.Matches(Content);
      for var Match in Matches do
        Result.Add(Match.Value);
    end;
    
    // Adicionar outros casos conforme necessário
  else
    // Fallback genérico
    var Lines := TStringList.Create;
    try
      Lines.Text := Content;
      for var Line in Lines do
      begin
        if Line.Contains('function') or Line.Contains('def ') or Line.Contains('procedure') then
          Result.Add(Line);
      end;
    finally
      Lines.Free;
    end;
  end;
end;

function TDataProcessor.ExtractComments(const Content: string; Language: TCodeLanguage): TStringList;
begin
  Result := TStringList.Create;
  
  case Language of
    clPascal:
    begin
      var Regex := TRegEx.Create('\{.*?\}|//.*?$|\(\*.*?\*\)', [roIgnoreCase, roMultiLine]);
      var Matches := Regex.Matches(Content);
      for var Match in Matches do
        Result.Add(Match.Value);
    end;
    
    clPython, clJavaScript:
    begin
      var Regex := TRegEx.Create('#.*?$|//.*?$|/\*.*?\*/', [roIgnoreCase, roMultiLine]);
      var Matches := Regex.Matches(Content);
      for var Match in Matches do
        Result.Add(Match.Value);
    end;
    
  else
    // Fallback
    Result.Add('// Comentário extraído');
  end;
end;

function TDataProcessor.AugmentTrainingData(const Items: TObjectList<TTrainingItem>): TObjectList<TTrainingItem>;
var
  I: Integer;
  AugmentedItems: TStringList;
  NewItem: TTrainingItem;
  J: Integer;
begin
  Result := TObjectList<TTrainingItem>.Create;
  
  for I := 0 to Items.Count - 1 do
  begin
    // Augmentar texto
    if FAugmentationOptions.UseParaphrasing then
    begin
      AugmentedItems := AugmentTextData(Items[I].Input);
      try
        for J := 0 to AugmentedItems.Count - 1 do
        begin
          NewItem := TTrainingItem.Create;
          NewItem.Input := AugmentedItems[J];
          NewItem.ExpectedOutput := Items[I].ExpectedOutput;
          NewItem.Metadata := TJSONObject(Items[I].Metadata.Clone);
          NewItem.Metadata.AddPair('augmented', TJSONBool.Create(True));
          
          Result.Add(NewItem);
        end;
      finally
        AugmentedItems.Free;
      end;
    end;
  end;
end;

function TDataProcessor.AugmentTextData(const OriginalText: string): TStringList;
begin
  Result := TStringList.Create;
  
  // Implementação simplificada de augmentação
  if FAugmentationOptions.UseSynonymReplacement then
  begin
    var AugmentedText := StringReplace(OriginalText, 'grande', 'enorme', [rfReplaceAll, rfIgnoreCase]);
    AugmentedText := StringReplace(AugmentedText, 'pequeno', 'diminuto', [rfReplaceAll, rfIgnoreCase]);
    Result.Add(AugmentedText);
  end;
  
  if FAugmentationOptions.UseParaphrasing then
  begin
    // Trocar ordem de algumas palavras (muito simplificado)
    var Words := OriginalText.Split([' ']);
    if Length(Words) > 3 then
    begin
      var Temp := Words[0];
      Words[0] := Words[1];
      Words[1] := Temp;
      Result.Add(string.Join(' ', Words));
    end;
  end;
end;

function TDataProcessor.AugmentCodeData(const OriginalCode: string; Language: TCodeLanguage): TStringList;
begin
  Result := TStringList.Create;
  
  // Implementação simplificada para augmentação de código
  if FAugmentationOptions.UseCodeRefactoring then
  begin
    // Trocar nomes de variáveis (muito básico)
    var AugmentedCode := StringReplace(OriginalCode, 'i', 'index', [rfReplaceAll]);
    AugmentedCode := StringReplace(AugmentedCode, 'j', 'counter', [rfReplaceAll]);
    Result.Add(AugmentedCode);
  end;
end;

procedure TDataProcessor.SetDefaultAugmentationOptions;
begin
  FAugmentationOptions.UseParaphrasing := True;
  FAugmentationOptions.UseSynonymReplacement := True;
  FAugmentationOptions.UseBackTranslation := False;
  FAugmentationOptions.UseCodeRefactoring := True;
  FAugmentationOptions.UseImageRotation := True;
  FAugmentationOptions.UseImageFlip := True;
  FAugmentationOptions.UseImageBrightness := False;
  FAugmentationOptions.AugmentationFactor := 0.5; // 50% mais dados
end;

// Implementações restantes dos métodos (placeholders)
function TDataProcessor.AnalyzeDataset(const Directory: string): TDataStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LanguageDistribution := TDictionary<string, Integer>.Create;
  Result.FileSizeDistribution := TDictionary<string, Integer>.Create;
end;

function TDataProcessor.GetDataQualityReport: string;
begin
  Result := 'Relatório de qualidade dos dados...\n\n';
  Result := Result + Format('Arquivos processados: %d\n', [FProcessedFiles.Count]);
  Result := Result + Format('Arquivos com erro: %d\n', [FErrorFiles.Count]);
  Result := Result + Format('Itens de treinamento gerados: %d\n', [FProcessedData.Count]);
end;

function TDataProcessor.ValidateDataIntegrity: Boolean;
begin
  Result := (FProcessedData.Count > 0) and (FErrorFiles.Count < FProcessedFiles.Count);
end;

procedure TDataProcessor.ApplyDataAugmentation(var Batches: TTrainingBatchList);
begin
  // Implementar augmentação em batches
end;

function TDataProcessor.LoadFromCache(const CacheFile: string): Boolean;
begin
  Result := False; // Placeholder
end;

function TDataProcessor.SaveToCache(const CacheFile: string): Boolean;
begin
  Result := False; // Placeholder
end;

procedure TDataProcessor.ClearCache;
begin
  FTrainingBatches.Clear;
  FValidationBatches.Clear;
  FProcessedData.Clear;
  FProcessedFiles.Clear;
  FErrorFiles.Clear;
end;

function TDataProcessor.GetSupportedFormats: TArray<string>;
begin
  Result := FFileFilters.ToStringArray;
end;

function TDataProcessor.EstimateProcessingTime(const Files: TStringList): TDateTime;
var
  TotalSize: Int64;
  I: Integer;
begin
  TotalSize := 0;
  for I := 0 to Files.Count - 1 do
  begin
    if FileExists(Files[I]) then
      TotalSize := TotalSize + TFile.GetSize(Files[I]);
  end;
  
  // Estimar baseado no tamanho (aproximadamente 1MB por segundo)
  Result := (TotalSize / (1024 * 1024)) / SecsPerDay;
end;

procedure TDataProcessor.AddFileFilter(const Extension: string);
begin
  if FFileFilters.IndexOf(Extension) < 0 then
    FFileFilters.Add(Extension);
end;

procedure TDataProcessor.RemoveFileFilter(const Extension: string);
var
  Index: Integer;
begin
  Index := FFileFilters.IndexOf(Extension);
  if Index >= 0 then
    FFileFilters.Delete(Index);
end;

procedure TDataProcessor.ConfigureForLanguage(Language: TCodeLanguage);
begin
  // Configurar filtros específicos para a linguagem
  case Language of
    clPascal:
    begin
      AddFileFilter('.pas');
      AddFileFilter('.pp');
      AddFileFilter('.dpr');
    end;
    clPython:
    begin
      AddFileFilter('.py');
      AddFileFilter('.pyw');
    end;
    // Adicionar outros casos conforme necessário
  end;
end;

end.