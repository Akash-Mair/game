module game_tech_test.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

type CellDto =
    {| IsOn: bool; Coordinates: {| XAxis: int; YAxis: int |} |}

type Cell =
    | Live of int * int
    | Dead of int * int
    member this.Coordinates = match this with Live (x,y) | Dead (x,y) -> (x,y)
    member this.Alive = match this with Live _ -> true | Dead _ -> false
        
    member this.Serialise () =
        match this with
        | Live (x,y) ->
            {| IsOn = true; Coordinates = {| XAxis = x; YAxis = y |} |}
        | Dead (x,y) ->
            {| IsOn = false; Coordinates = {| XAxis = x; YAxis = y |} |}
    
    static member Deserialise (cellDto: CellDto) =
        let coords = cellDto.Coordinates.XAxis, cellDto.Coordinates.YAxis
        if cellDto.IsOn then
            Live coords
        else
            Dead coords 
            
type GridDto =
    {|
        Grid: CellDto array 
    |}

type Grid =
    | Grid of Cell list
    
    member this.Cells = match this with Grid cells -> cells
    
    member this.Serialise () =
        {| Grid = this.Cells |> List.map (fun cell -> cell.Serialise()) |> List.toArray |}
    
    static member Deserialise (gridDto: GridDto)  =
        gridDto.Grid
        |> Array.map Cell.Deserialise
        |> List.ofArray
        |> Grid 


let initGrid gridLength =
    let xAxis = [0..gridLength-1]
    let yAxis = [0..gridLength-1]
    List.allPairs xAxis yAxis
    |> List.map Dead
    |> Grid 

let getAllPossibleNeighbours (x,y) =
    [
        x-1, y-1
        x-1, y+1
        x-1, y 
        x, y+1
        x, y-1 
        x+1, y 
        x+1, y+1
        x+1, y-1
    ]

let findExistingNeighbours (grid: Grid) (possibleNeighbours: (int * int) list) =
    grid.Cells
    |> List.choose (fun cell ->
        if possibleNeighbours |> List.contains cell.Coordinates then
            Some cell
        else None)
    
let switchCell predicate cell =
    if predicate then
        match cell with     
        | Dead (x,y) -> Live (x,y)
        | Live (x,y) -> Dead (x,y)
    else
        cell

let (|Survive|Heal|Die|) (cell: Cell, grid) =
    let allLiveNeighbours =
        cell.Coordinates
        |> getAllPossibleNeighbours
        |> findExistingNeighbours grid
        |> List.filter (fun cell -> cell.Alive)
    match cell with
    | Live _ ->
        if (allLiveNeighbours.Length = 2 || allLiveNeighbours.Length = 3) then Survive else Die 
    | Dead _ ->
        if (allLiveNeighbours.Length = 3) then Heal else Die 


let randomlySwitchCells (grid: Grid) =
    let rand = System.Random()
    let trySwitch cell =
        switchCell (rand.Next(1,101) < 30) cell 
    grid.Cells
    |> List.map trySwitch
    |> Grid 

let runTick (grid: Grid) =
    grid.Cells
    |> List.map (fun cell ->
        match cell, grid with
        | Survive
        | Heal -> Live cell.Coordinates
        | Die -> Dead cell.Coordinates
        )
    |> Grid 


let gameState = ResizeArray<Grid>()

gameState.Add(initGrid 4)

let updateGame grid =
    gameState.RemoveAll(fun _ -> true) |> ignore 
    gameState.Add(grid) 

let setGridHandler: HttpHandler =
    fun next ctx ->
        task {
            let! gridDto = ctx.BindJsonAsync<GridDto>()
            let grid = Grid.Deserialise gridDto
            updateGame grid 
            return! Successful.CREATED gridDto next ctx 
        }
        
let tickHandler: HttpHandler =
    fun next ctx ->
        task {
            let newGrid = runTick gameState.[0]
            updateGame newGrid
            return! json (newGrid.Serialise()) next ctx 
        }

let getGridHandler: HttpHandler =
    fun next ctx ->
        task {
            let gridDto = gameState.[0].Serialise()
            return! json gridDto next ctx 
        }


let webApp =
    choose [
        POST >=> route "/" >=> setGridHandler 
        GET >=>
            choose [
                route "/tick" >=> tickHandler
                route "/" >=> getGridHandler
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0